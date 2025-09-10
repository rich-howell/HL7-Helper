import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';

/* =========================
 * Types
 * ========================= */
type SegmentFields = Record<string, { name: string; datatype?: string }>;
type SegmentsIndex = Record<string, SegmentFields>;
type DatatypeMap = Record<string, string>; // supports "4.1" etc.
type DatatypesIndex = Record<string, DatatypeMap>;

type ParsedIndexCall = {
  kind: 'index';
  seg: string;
  segIdx: string;
  field: string;
  rep: string;
  comp: string;
  sub: string;
  rawArgs: string[];
};

type ParsedPathCall = {
  kind: 'path';
  seg: string;
  segIdx: string;
  field?: string;
  rep?: string;
  comp?: string;
  sub?: string;
  rawArgs: string[];
};

type ParsedCall = ParsedIndexCall | ParsedPathCall;

/* =========================
 * Helpers
 * ========================= */
function getEffIndexAtPosition(
  doc: vscode.TextDocument,
  pos: vscode.Position,
  fnRe: RegExp,
  wrapRe: RegExp
): number | null {
  // Try the open-call prefix first (no closing ')')
  const open = findOpenCallPrefixAroundPosition(doc, pos, fnRe, wrapRe);
  if (open) {
    const { argIndex, firstArgText } = readPartialArgs(open.textUpToCursor);
    const isWrapper = !!firstArgText && IDENT_RE.test(firstArgText);
    return isWrapper ? argIndex - 1 : argIndex;
  }

  // Then try balanced (closed) call and count commas only up to the caret
  const slice = findEnclosingCallAroundPosition(doc, pos, fnRe, wrapRe);
  if (!slice) return null;

  const ps = slice.text.indexOf('(');
  const pe = slice.text.lastIndexOf(')');
  if (ps < 0 || pe <= ps) return null;

  const off = offsetInSlice(doc, slice, pos);
  // Count commas strictly inside (...) and before the caret
  let argIndex = 0;
  for (let i = ps + 1; i < Math.min(off, pe); i++) {
    if (slice.text[i] === ',') argIndex++;
  }

  // Wrapper offset: peek first argument in the whole call
  let wrapperOffset = 0;
  {
    const inner = slice.text.slice(ps + 1, pe).trim();
    const firstComma = inner.indexOf(',');
    const first = (firstComma === -1 ? inner : inner.slice(0, firstComma)).trim();
    if (first && IDENT_RE.test(first)) wrapperOffset = 1;
  }

  return argIndex - wrapperOffset;
}

// Replace the numeric token under the cursor (contiguous digits). If none, insert at cursor.
function findNumericTokenReplaceRange(doc: vscode.TextDocument, pos: vscode.Position): vscode.Range {
  const line = doc.lineAt(pos.line).text;
  let start = pos.character, end = pos.character;

  // expand left
  while (start > 0 && /\d/.test(line[start - 1])) start--;
  // expand right
  while (end < line.length && /\d/.test(line[end])) end++;

  return new vscode.Range(new vscode.Position(pos.line, start), new vscode.Position(pos.line, end));
}

// Find the range inside the current string literal to replace (from the opening quote to the cursor).
function findStringInnerReplaceRange(doc: vscode.TextDocument, pos: vscode.Position): vscode.Range | null {
  const lineText = doc.lineAt(pos.line).text;
  let i = pos.character - 1;
  // Walk left to find the nearest non-escaped quote
  while (i >= 0) {
    const ch = lineText[i];
    if (ch === '"' || ch === "'" || ch === '`') {
      // if escaped like \", skip it
      const prev = i > 0 ? lineText[i - 1] : '';
      if (prev !== '\\') {
        const start = new vscode.Position(pos.line, i + 1); // just after the opening quote
        return new vscode.Range(start, pos);
      }
    }
    i--;
  }
  return null;
}

function debounce<F extends (...args: any[]) => void>(fn: F, ms: number) {
  let t: NodeJS.Timeout | undefined;
  return (...args: Parameters<F>) => {
    if (t) clearTimeout(t);
    t = setTimeout(() => fn(...args), ms);
  };
}

function loadJson<T>(context: vscode.ExtensionContext, rel: string): T | null {
  try {
    const p = path.join(context.extensionPath, rel);
    const txt = fs.readFileSync(p, 'utf8');
    return JSON.parse(txt) as T;
  } catch (e) {
    console.warn(`[hl7-hover] Failed to load ${rel}:`, e);
    return null;
  }
}

// Build a regex that matches: <anyVar>.<oneOfFnNames>(
function buildFnCallRegex(fnNames: string[]): RegExp {
  const fnGroup = fnNames.map(f => f.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')).join('|');
  return new RegExp(String.raw`(?<var>[A-Za-z_$][A-Za-z0-9_$]*)\s*\.\s*(?<fn>${fnGroup})\s*\(`, 'g');
}

// Matches bare helper calls like: hl7.getValue(msg, "PID", 0, 3, 0, 1, 1) OR getValue(msg, "/PID(0)-3(0)-1(1)")
function buildWrapperFnCallRegex(fnNames: string[]): RegExp {
  const fnGroup = fnNames.map(f => f.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')).join('|');
  // optional namespace + dot, or none: (hl7.)?getValue(
  return new RegExp(String.raw`(?:(?<ns>[A-Za-z_$][A-Za-z0-9_$]*)\s*\.\s*)?(?<fn>${fnGroup})\s*\(`, 'g');
}

// For token targeting / completion context
const NUMBER_OR_SEGSTR_RE = /\d+|["'`][A-Z]{3}["'`]|["'`](?:\\.|[^"'`])*["'`]/g;
const IDENT_RE = /^[A-Za-z_$][A-Za-z0-9_$]*$/;

// HL7 path like /PID(0)-3(0)-1(1)
const HL7_PATH_RE =
  /^\/?(?<seg>[A-Z]{3})\((?<segIdx>\d+)\)(?:-(?<field>\d+)\((?<rep>\d+)\))?(?:-(?<comp>\d+)\((?<sub>\d+)\))?/;

// Extract entire call (member or wrapper) from a single line given the index of '('
function sliceCallWithBalancedParens(line: string, openParenIdx: number): { start: number; end: number } | null {
  let depth = 0;
  let end = -1;
  for (let i = openParenIdx; i < line.length; i++) {
    const ch = line[i];
    if (ch === '(') depth++;
    else if (ch === ')') {
      depth--;
      if (depth === 0) { end = i; break; }
    }
  }
  if (end === -1) return null;

  // Walk left to include "<var>.<fn>" OR "(ns.)?fn"
  let start = openParenIdx - 1;
  while (start > 0 && /\s/.test(line[start])) start--;
  while (start > 0 && /[A-Za-z0-9_$\.]/.test(line[start - 1])) start--;
  return { start, end };
}

// Multiline-safe extraction of a call starting at (lineNum, openParenCol)
function sliceCallFromDocument(
  doc: vscode.TextDocument,
  lineNum: number,
  openParenColumn: number
): { start: vscode.Position; end: vscode.Position; text: string } | null {
  let depth = 0;
  const start = new vscode.Position(lineNum, openParenColumn);
  for (let ln = lineNum; ln < doc.lineCount; ln++) {
    const text = doc.lineAt(ln).text;
    const fromCol = (ln === lineNum ? openParenColumn : 0);
    for (let col = fromCol; col < text.length; col++) {
      const ch = text[col];
      if (ch === '(') depth++;
      else if (ch === ')') {
        depth--;
        if (depth === 0) {
          const end = new vscode.Position(ln, col);
          // Collect text from start to end (inclusive)
          let buf = '';
          for (let i = start.line; i <= end.line; i++) {
            const t = doc.lineAt(i).text;
            if (i === start.line && i === end.line) buf += t.slice(start.character, end.character + 1);
            else if (i === start.line) buf += t.slice(start.character) + '\n';
            else if (i === end.line) buf += t.slice(0, end.character + 1);
            else buf += t + '\n';
          }
          return { start, end, text: buf };
        }
      }
    }
  }
  return null;
}

// Parse either:
//  A) index style: "PID", segIdx, field, rep, comp, sub
//  B) path style:  "/PID(0)-3(0)-1(1)"
function parseCallSignature(callText: string): ParsedCall | null {
  const parenStart = callText.indexOf('(');
  const parenEnd = callText.lastIndexOf(')');
  if (parenStart < 0 || parenEnd < 0 || parenEnd <= parenStart) return null;
  const argsText = callText.slice(parenStart + 1, parenEnd);

  const rawArgs = argsText.split(',').map(s => s.trim());

  // (B) Path-style first: "/PID(0)-3(0)-1(1)"
  if (rawArgs.length >= 1 && /^["'`]/.test(rawArgs[0])) {
    const pathArg = rawArgs[0];
    const isSegLiteral = /^["'`][A-Z]{3}["'`]$/.test(pathArg); // e.g. "PID"
    if (!isSegLiteral) {
      const pathStr = pathArg.slice(1, -1);
      const m = pathStr.match(HL7_PATH_RE);
      if (m && m.groups) {
        const g = m.groups as {
          seg?: string; segIdx?: string; field?: string; rep?: string; comp?: string; sub?: string;
        };
        const { seg, segIdx, field, rep, comp, sub } = g;
        if (!seg || !segIdx) return null; // required
        return { kind: 'path', seg, segIdx, field, rep, comp, sub, rawArgs };
      }
    }
  }

  // (A) Index-style: "PID",0,3,0,1,1  (need at least 6)
  if (rawArgs.length >= 6 && /^["'`][A-Z]{3}["'`]$/.test(rawArgs[0])) {
    const seg = rawArgs[0].slice(1, -1);
    const [, segIdx, field, rep, comp, sub] = rawArgs;
    return { kind: 'index', seg, segIdx, field, rep, comp, sub, rawArgs };
  }

  return null;
}

function formatPath(seg?: string, field?: string, comp?: string, sub?: string): string {
  let p = seg ? seg : '';
  if (field) p += `-${field}`;
  if (comp)  p += `.${comp}`;
  if (sub)   p += `.${sub}`;
  return p || '(unknown)';
}

// Compare positions
function posLE(a: vscode.Position, b: vscode.Position) {
  return a.line < b.line || (a.line === b.line && a.character <= b.character);
}
function posWithin(start: vscode.Position, end: vscode.Position, p: vscode.Position) {
  return posLE(start, p) && posLE(p, end);
}

// Find the enclosing HL7 call around the cursor (member or wrapper), scanning nearby lines
function findEnclosingCallAroundPosition(
  doc: vscode.TextDocument,
  pos: vscode.Position,
  fnRe: RegExp,
  wrapRe: RegExp
): { start: vscode.Position; end: vscode.Position; text: string } | null {
  const from = Math.max(0, pos.line - 20);
  const to   = Math.min(doc.lineCount - 1, pos.line + 20);

  // First pass: member form var.fn(...)
  for (let ln = from; ln <= to; ln++) {
    const line = doc.lineAt(ln).text;
    fnRe.lastIndex = 0;
    let m: RegExpExecArray | null;
    while ((m = fnRe.exec(line))) {
      const openParenIdx = m.index + m[0].length - 1; // '('
      const slice = sliceCallFromDocument(doc, ln, openParenIdx);
      if (!slice) continue;
      if (posWithin(slice.start, slice.end, pos)) return slice;
    }
  }

  // Second pass: wrapper form (ns.)?fn(var, ...)
  for (let ln = from; ln <= to; ln++) {
    const line = doc.lineAt(ln).text;
    wrapRe.lastIndex = 0;
    let m: RegExpExecArray | null;
    while ((m = wrapRe.exec(line))) {
      const openParenIdx = m.index + m[0].length - 1; // '('
      const slice = sliceCallFromDocument(doc, ln, openParenIdx);
      if (!slice) continue;

      // Validate first arg looks like an identifier
      const ps = slice.text.indexOf('('), pe = slice.text.lastIndexOf(')');
      if (ps < 0 || pe <= ps) continue;
      const firstArg = (slice.text.slice(ps + 1, pe).split(',')[0] || '').trim();
      if (!IDENT_RE.test(firstArg)) continue;

      if (posWithin(slice.start, slice.end, pos)) return slice;
    }
  }

  return null;
}

// Compute cursor offset within the concatenated slice.text (for token hovers/completions)
function offsetInSlice(doc: vscode.TextDocument, slice: { start: vscode.Position; end: vscode.Position; text: string }, pos: vscode.Position): number {
  let offset = 0;
  for (let ln = slice.start.line; ln < pos.line; ln++) {
    const t = doc.lineAt(ln).text;
    const startCol = (ln === slice.start.line) ? slice.start.character : 0;
    offset += t.length - startCol;
    offset += 1; // we inserted '\n' between lines in sliceCallFromDocument
  }
  const startColThis = (pos.line === slice.start.line) ? slice.start.character : 0;
  offset += (pos.character - startColThis);
  return Math.max(0, Math.min(offset, slice.text.length));
}

/* ---------- NEW (for autocomplete): tolerant, partial call parsing ---------- */

// Find an OPEN call up to the cursor (no need for closing ')'), member+wrapper
function findOpenCallPrefixAroundPosition(
  doc: vscode.TextDocument,
  pos: vscode.Position,
  fnRe: RegExp,
  wrapRe: RegExp
): { start: vscode.Position; textUpToCursor: string } | null {
  const from = Math.max(0, pos.line - 20);
  const to   = Math.min(doc.lineCount - 1, pos.line + 20);

  function collectFrom(start: vscode.Position): string {
    let buf = '';
    for (let ln = start.line; ln <= pos.line; ln++) {
      const t = doc.lineAt(ln).text;
      if (ln === start.line && ln === pos.line) buf += t.slice(start.character, pos.character);
      else if (ln === start.line) buf += t.slice(start.character) + '\n';
      else if (ln === pos.line) buf += t.slice(0, pos.character);
      else buf += t + '\n';
    }
    return buf;
  }

  function better(a: vscode.Position | null, b: vscode.Position): boolean {
    if (!a) return true;
    if (b.line > a.line) return true;
    if (b.line < a.line) return false;
    return b.character > a.character;
  }

  let bestStart: vscode.Position | null = null;

  // member form
  for (let ln = from; ln <= to; ln++) {
    const line = doc.lineAt(ln).text;
    fnRe.lastIndex = 0;
    let m: RegExpExecArray | null;
    while ((m = fnRe.exec(line))) {
      const openParenIdx = m.index + m[0].length - 1; // '('
      // cursor must be after this '('
      if (pos.line < ln || (pos.line === ln && pos.character < openParenIdx)) continue;
      const candidate = new vscode.Position(ln, openParenIdx);
      if (better(bestStart, candidate)) bestStart = candidate;
    }
  }

  // wrapper form
  for (let ln = from; ln <= to; ln++) {
    const line = doc.lineAt(ln).text;
    wrapRe.lastIndex = 0;
    let m: RegExpExecArray | null;
    while ((m = wrapRe.exec(line))) {
      const openParenIdx = m.index + m[0].length - 1; // '('
      if (pos.line < ln || (pos.line === ln && pos.character < openParenIdx)) continue;
      const candidate = new vscode.Position(ln, openParenIdx);

      // validate first-arg-ish only if this candidate would beat current best
      if (better(bestStart, candidate)) {
        const prefix = collectFrom(candidate);
        const commaIdx = prefix.indexOf(',');
        const inner = commaIdx === -1 ? prefix.slice(1) : prefix.slice(1, commaIdx);
        const firstArg = (inner || '').trim();
        if (!firstArg || IDENT_RE.test(firstArg)) {
          bestStart = candidate;
        }
      }
    }
  }

  if (!bestStart) return null;
  return { start: bestStart, textUpToCursor: collectFrom(bestStart) };
}

// Lightweight partial arg reader for completion (ignores commas inside quotes)
type QuoteChar = '"' | "'" | '`';

function readPartialArgs(textUpToCursor: string) {
  const ps = textUpToCursor.indexOf('(');
  if (ps < 0) return { argIndex: 0, firstArgText: '', args: [] as string[] };

  const inner = textUpToCursor.slice(ps + 1); // from after '(' to cursor
  const args: string[] = [];
  let buf = '';
  let quote: QuoteChar | null = null;
  let escaped = false;

  for (let i = 0; i < inner.length; i++) {
    const ch = inner[i];

    if (quote) {
      // inside a quoted string
      buf += ch;
      if (escaped) {
        escaped = false;
      } else if (ch === '\\') {
        escaped = true;
      } else if (ch === quote) {
        quote = null;
      }
      continue;
    }

    // not in quotes
    if (ch === '"' || ch === "'" || ch === '`') {
      quote = ch as QuoteChar;
      buf += ch;
      continue;
    }

    if (ch === ',') {
      args.push(buf.trim());
      buf = '';
      continue;
    }

    buf += ch;
  }
  args.push(buf.trim());

  const argIndex = Math.max(0, args.length - 1);
  const firstArgText = args[0] ?? '';
  return { argIndex, firstArgText, args };
}

/* =========================
 * Activate
 * ========================= */
export function activate(context: vscode.ExtensionContext) {

  const config = vscode.workspace.getConfiguration();

  // Settings
  const version = config.get<string>('hl7Helper.version', 'v251');
  const specBaseUrl = config.get<string>('hl7Helper.specBaseUrl', 'https://hl7-definition.caristix.com/v2');
  const cfgNames = config.get<string[]>('hl7Helper.fnNames', ['getValue', 'setValue', 'getValues', 'setValues']);
  const fnNames = Array.from(new Set([...cfgNames, 'segmentExists']));

  // Version → Caristix path segment
  const versionPathMap: Record<string, string> = {
    v231: 'HL7v2.3.1',
    v240: 'HL7v2.4',
    v251: 'HL7v2.5.1',
    v27:  'HL7v2.7'
  };
  const versionPath = versionPathMap[version] ?? 'HL7v2.5.1';

  // Load dictionaries (empty objects if missing)
  const segments = loadJson<SegmentsIndex>(context, `data/segments.${version}.json`) ?? {};
  const dtypes   = loadJson<DatatypesIndex>(context, `data/datatypes.${version}.json`) ?? {};

  // Dynamic matchers
  const FN_CALL_RE = buildFnCallRegex(fnNames);
  const WRAP_FN_CALL_RE = buildWrapperFnCallRegex(fnNames);

  // --- Auto-trigger on cursor movement into HL7 slots (segment/field/component)
const triggerSuggestOnSelection = debounce(() => {
  const ed = vscode.window.activeTextEditor;
  if (!ed) return;

  const doc = ed.document;
  const pos = ed.selection.active;
  const lang = doc.languageId;
  if (!['javascript','typescript','javascriptreact','typescriptreact'].includes(lang)) return;

  const effIndex = getEffIndexAtPosition(doc, pos, FN_CALL_RE, WRAP_FN_CALL_RE);
  if (effIndex === 0 || effIndex === 2 || effIndex === 4) {
    void vscode.commands.executeCommand('editor.action.triggerSuggest');
  }
}, 80);

// fire when the selection changes or editor focus changes
context.subscriptions.push(
  vscode.window.onDidChangeTextEditorSelection(() => triggerSuggestOnSelection()),
  vscode.window.onDidChangeActiveTextEditor(() => triggerSuggestOnSelection())
);


  /* ---------- Hover Provider ---------- */
  const hoverProvider: vscode.HoverProvider = {
    provideHover(doc, pos) {
      const slice = findEnclosingCallAroundPosition(doc, pos, FN_CALL_RE, WRAP_FN_CALL_RE);
      if (!slice) return;

      const callText = slice.text;
      
      // --- Minimal hover for calls shaped like ("SEG", idx) ---
      // Works for member or wrapper. Returns only if the shape matches; otherwise
      // we fall through to your original hover logic.
      {
        const openParen = callText.indexOf('(');
        const closeParen = callText.lastIndexOf(')');
        if (openParen >= 0 && closeParen > openParen) {
          const cursorOffset = offsetInSlice(doc, slice, pos);

          // 1) Parse first arg: quoted segment literal (support A-Z0-9 e.g. "AL1", "Z99")
          let p = openParen + 1;
          while (p < closeParen && /\s/.test(callText[p])) p++;
          const quote = callText[p];
          if (quote === '"' || quote === '\'' || quote === '`') {
            const segStart = p;
            const segEnd = callText.indexOf(quote, segStart + 1);
            if (segEnd > segStart) {
              const segLit = callText.slice(segStart, segEnd + 1);
              const segOk = /^["'`][A-Z0-9]{3}["'`]$/.test(segLit);

              // 2) Find comma
              p = segEnd + 1;
              while (p < closeParen && /\s/.test(callText[p])) p++;
              if (p < closeParen && callText[p] === ',') p++;
              while (p < closeParen && /\s/.test(callText[p])) p++;

              // 3) Parse second arg: integer index
              const idxStart = p;
              while (p < closeParen && /[-0-9]/.test(callText[p])) p++;
              const idxEnd = p;
              const idxLit = callText.slice(idxStart, idxEnd);
              const idxOk = /^-?\d+$/.test(idxLit);

              if (segOk && idxOk) {
                const segCode = segLit.slice(1, -1).toUpperCase();

                // Hover over the quoted segment literal
                if (cursorOffset >= segStart && cursorOffset <= segEnd + 1) {
                  const md = new vscode.MarkdownString(); md.isTrusted = true;
                  md.appendMarkdown(`**${segCode}** segment`);
                  const specUrl = `${specBaseUrl}/${versionPath}/Segments/${segCode}`;
                  md.appendMarkdown(`\n\n[Open ${segCode} spec](${specUrl})`);
                  return new vscode.Hover(md);
                }

                // Hover over the index number
                if (cursorOffset >= idxStart && cursorOffset <= idxEnd) {
                  const md = new vscode.MarkdownString(); md.isTrusted = true;
                  md.appendMarkdown(`**${segCode}[${idxLit}]** segment occurrence`);
                  return new vscode.Hover(md);
                }
              }
            }
          }
        }
      }
      // --- end minimal ("SEG", idx) hover ---

      const parsed = parseCallSignature(callText);
      if (!parsed) return;

      const { seg, field, comp, sub } = parsed;
      const segFields = seg ? segments[seg] ?? {} : {};
      const fld = field ? segFields[field] : undefined;
      const fldName = fld?.name || (field ? `Field ${field}` : 'Field');
      const dtype = fld?.datatype;

      // token under cursor
      const cursorOffset = offsetInSlice(doc, slice, pos);
      let overToken = '';
      NUMBER_OR_SEGSTR_RE.lastIndex = 0;
      let tok: RegExpExecArray | null;
      while ((tok = NUMBER_OR_SEGSTR_RE.exec(callText))) {
        const ts = tok.index ?? 0;
        const te = ts + tok[0].length;
        if (cursorOffset >= ts && cursorOffset <= te) { overToken = tok[0]; break; }
      }

      const md = new vscode.MarkdownString(); md.isTrusted = true;

      if (/^["'`][A-Z]{3}["'`]$/.test(overToken) && seg) {
        md.appendMarkdown(`**${seg}** segment`);
        return new vscode.Hover(md);
      }
      if (/^\d+$/.test(overToken) && field && overToken === field) {
        md.appendMarkdown(`**${seg}-${field}** ${fldName}${dtype ? ` _(datatype ${dtype})_` : ''}`);
        return new vscode.Hover(md);
      }
      if (/^\d+$/.test(overToken) && dtype && (overToken === comp || overToken === sub)) {
        const dt = dtypes[dtype] || {};
        if (overToken === comp) {
          const compName = dt[comp!] || `Component ${comp}`;
          md.appendMarkdown(`**${dtype}.${comp}** ${compName}`);
          return new vscode.Hover(md);
        }
        if (overToken === sub) {
          const subKey = `${comp}.${sub}`;
          const subName = dt[subKey] || `Subcomponent ${sub}`;
          md.appendMarkdown(`**${dtype}.${comp}.${sub}** ${subName}`);
          return new vscode.Hover(md);
        }
      }

      // Fallback summary
      const md2 = new vscode.MarkdownString(); md2.isTrusted = true;
      md2.appendMarkdown(`**Path:** ${formatPath(seg, field, comp, sub)}`);
      if (dtype) md2.appendMarkdown(`\n\nDatatype: **${dtype}**`);
      return new vscode.Hover(md2);
    }
  };

  context.subscriptions.push(
    vscode.languages.registerHoverProvider(
      [
        { language: 'javascript' }, { language: 'typescript' },
        { language: 'javascriptreact' }, { language: 'typescriptreact' }
      ],
      hoverProvider
    )
  );

  /* ---------- CodeLens Provider ---------- */
  class HL7CodeLensProvider implements vscode.CodeLensProvider {
    provideCodeLenses(doc: vscode.TextDocument): vscode.CodeLens[] {
      const lenses: vscode.CodeLens[] = [];

      for (let ln = 0; ln < doc.lineCount; ln++) {
        const text = doc.lineAt(ln).text;

        // member-form
        FN_CALL_RE.lastIndex = 0;
        let m: RegExpExecArray | null;
        while ((m = FN_CALL_RE.exec(text))) {
          const openParenIdx = m.index + m[0].length - 1;
          const docSlice = sliceCallFromDocument(doc, ln, openParenIdx);
          if (!docSlice) continue;

          const parsed = parseCallSignature(docSlice.text);
          if (!parsed) continue;

          const { seg, field } = parsed;
          const segFields = seg ? segments[seg] ?? {} : {};
          const fld = field ? segFields[field] : undefined;
          const fldName = fld?.name || (field ? `Field ${field}` : 'Field');
          const dtype = fld?.datatype;

          const baseRange = new vscode.Range(docSlice.start, docSlice.end.translate(0, 1));

          lenses.push(new vscode.CodeLens(baseRange, {
            title: `${seg}${field ? `-${field}` : ''} ${fldName}${dtype ? ` (${dtype})` : ''} — Open Spec`,
            command: 'hl7Helper.openSegmentSpec',
            arguments: [seg]
          }));

          if (dtype) {
            lenses.push(new vscode.CodeLens(baseRange, {
              title: `${dtype} — Open Datatype`,
              command: 'hl7Helper.openDatatypeSpec',
              arguments: [dtype]
            }));
          }
        }

        // wrapper-form
        WRAP_FN_CALL_RE.lastIndex = 0;
        while ((m = WRAP_FN_CALL_RE.exec(text))) {
          const openParenIdx = m.index + m[0].length - 1;
          const docSlice = sliceCallFromDocument(doc, ln, openParenIdx);
          if (!docSlice) continue;

          // Validate first arg is an identifier
          const ps = docSlice.text.indexOf('('), pe = docSlice.text.lastIndexOf(')');
          if (ps < 0 || pe <= ps) continue;
          const firstArg = (docSlice.text.slice(ps + 1, pe).split(',')[0] || '').trim();
          if (!IDENT_RE.test(firstArg)) continue;

          const parsed = parseCallSignature(docSlice.text);
          if (!parsed) continue;

          const { seg, field } = parsed;
          const segFields = seg ? segments[seg] ?? {} : {};
          const fld = field ? segFields[field] : undefined;
          const fldName = fld?.name || (field ? `Field ${field}` : 'Field');
          const dtype = fld?.datatype;

          const baseRange = new vscode.Range(docSlice.start, docSlice.end.translate(0, 1));

          lenses.push(new vscode.CodeLens(baseRange, {
            title: `${seg}${field ? `-${field}` : ''} ${fldName}${dtype ? ` (${dtype})` : ''} — Open Spec`,
            command: 'hl7Helper.openSegmentSpec',
            arguments: [seg]
          }));
          if (dtype) {
            lenses.push(new vscode.CodeLens(baseRange, {
              title: `${dtype} — Open Datatype`,
              command: 'hl7Helper.openDatatypeSpec',
              arguments: [dtype]
            }));
          }
        }
      }

      return lenses;
    }
  }

  context.subscriptions.push(
    vscode.languages.registerCodeLensProvider(
      [
        { language: 'javascript' }, { language: 'typescript' },
        { language: 'javascriptreact' }, { language: 'typescriptreact' }
      ],
      new HL7CodeLensProvider()
    )
  );

  /* ---------- Diagnostics (out-of-range checks) ---------- */
  const diagCollection = vscode.languages.createDiagnosticCollection('hl7Helper');
  context.subscriptions.push(diagCollection);

  function validateDoc(doc: vscode.TextDocument) {
    const diagnostics: vscode.Diagnostic[] = [];

    for (let ln = 0; ln < doc.lineCount; ln++) {
      const text = doc.lineAt(ln).text;

      // Check both forms on this line
      for (const re of [FN_CALL_RE, WRAP_FN_CALL_RE]) {
        re.lastIndex = 0;
        let m: RegExpExecArray | null;
        while ((m = re.exec(text))) {
          const openParenIdx = m.index + m[0].length - 1;
          const singleLine = sliceCallWithBalancedParens(text, openParenIdx);
          if (!singleLine) continue;

          const callText = text.slice(singleLine.start, singleLine.end + 1);
          // Wrapper sanity: ensure first arg is an identifier if wrapper-like
          if (re === WRAP_FN_CALL_RE) {
            const ps = callText.indexOf('('), pe = callText.lastIndexOf(')');
            if (ps < 0 || pe <= ps) continue;
            const firstArg = (callText.slice(ps + 1, pe).split(',')[0] || '').trim();
            if (!IDENT_RE.test(firstArg)) continue;
          }

          const parsed = parseCallSignature(callText);
          if (!parsed) continue;

          const { seg, field, comp } = parsed;
          if (!seg) continue;

          const segFields = segments[seg] || {};
          const range = new vscode.Range(
            new vscode.Position(ln, singleLine.start),
            new vscode.Position(ln, singleLine.end + 1)
          );

          if (field && !segFields[field]) {
            diagnostics.push(new vscode.Diagnostic(
              range,
              `${seg}-${field} is not defined in ${version} dictionary.`,
              vscode.DiagnosticSeverity.Warning
            ));
            continue;
          }

          const dtype = field ? segFields[field]?.datatype : undefined;
          if (dtype && comp) {
            const dt = dtypes[dtype] || {};
            if (!dt[comp]) {
              diagnostics.push(new vscode.Diagnostic(
                range,
                `${dtype}.${comp} not defined in ${version} datatype map.`,
                vscode.DiagnosticSeverity.Warning
              ));
            }
          }
        }
      }
    }

    diagCollection.set(doc.uri, diagnostics);
  }

  // Validate open docs & on changes
  if (vscode.window.activeTextEditor) validateDoc(vscode.window.activeTextEditor.document);
  context.subscriptions.push(vscode.workspace.onDidChangeTextDocument(e => validateDoc(e.document)));
  context.subscriptions.push(vscode.workspace.onDidOpenTextDocument(doc => validateDoc(doc)));
  context.subscriptions.push(vscode.workspace.onDidCloseTextDocument(doc => diagCollection.delete(doc.uri)));

  /* ---------- Completion Provider (Autocomplete) ---------- */
  const completionProvider: vscode.CompletionItemProvider = {
    provideCompletionItems(doc, pos) {
      // 1) Try tolerant open-call parsing (works mid-typing with no ')')
      const open = findOpenCallPrefixAroundPosition(doc, pos, FN_CALL_RE, WRAP_FN_CALL_RE);
      if (open) {
        const { argIndex, firstArgText, args } = readPartialArgs(open.textUpToCursor);

        // Wrapper detection: if first arg looks like an identifier -> wrapper call
        const isWrapper = !!firstArgText && IDENT_RE.test(firstArgText);

        // For logic below, use "effective" arg index where segment is always index 0,
        // field is 2, component is 4 (same as member form)
        const effIndex = isWrapper ? argIndex - 1 : argIndex;

        // Segment text source depends on wrapper/member:
        // member: firstArgText; wrapper: args[1]
        const segArgText = isWrapper ? (args[1] ?? '') : firstArgText;

        // detect partial/full segment literal in segArgText, e.g. "P  "PI  "PID
        let segPrefix: string | undefined;
        let segFromFirstArg: string | undefined;
        {
          const mFull = segArgText.match(/^["'`]([A-Za-z]{3})["'`]$/);
          const mPartial = segArgText.match(/^["'`]([A-Za-z]{0,3})$/); // no closing quote yet
          if (mFull) {
            segFromFirstArg = mFull[1].toUpperCase();
            segPrefix = segFromFirstArg;
          } else if (mPartial) {
            segPrefix = (mPartial[1] ?? '').toUpperCase();
          }
        }

        const items: vscode.CompletionItem[] = [];
        const add = (label: string, detail?: string, insert?: string) => {
          const it = new vscode.CompletionItem(label, vscode.CompletionItemKind.Value);
          if (detail) it.detail = detail;
          if (insert) it.insertText = insert;
          it.sortText = '0_' + label; // nudge ours to the top
          return items.push(it);
        };

        // Arg 0 → suggest segments
        if (effIndex === 0) {
          const inQuotes = /^["'`]/.test(segArgText); // typing inside "... or '... or `...
          const replaceRange = inQuotes ? findStringInnerReplaceRange(doc, pos) : undefined;

          Object.keys(segments)
            .filter(s => !segPrefix || s.startsWith(segPrefix))
            .forEach(s => {
              // If inside quotes, label without quotes so typing "P" matches PID.
              // If not inside quotes, insert the full quoted literal for convenience.
              const label = inQuotes ? s : `"${s}"`;
              const insert = inQuotes ? s : `"${s}"`;

              const it = new vscode.CompletionItem(label, vscode.CompletionItemKind.Value);
              it.detail = 'Segment';
              it.insertText = insert;
              it.filterText = s;        // critical so typing P matches PID (not "PID")
              it.sortText  = '0_' + s;

              if (replaceRange) {
                // Replace only the string contents (between the quotes)
                it.range = replaceRange;
              }
              items.push(it);
            });

          return items;
        }

        // If we can see the segment literal, we can suggest fields (argIndex === 2 for index-style)
        if (segFromFirstArg && effIndex === 2) {
          const replaceRange = findNumericTokenReplaceRange(doc, pos);
          const currentNum = doc.getText(replaceRange).trim();
          const fields = segments[segFromFirstArg] ?? {};

          const items: vscode.CompletionItem[] = [];
          Object.entries(fields)
            .sort(([a],[b]) => Number(a) - Number(b))
            .forEach(([num, meta]) => {
              const it = new vscode.CompletionItem(`${num}`, vscode.CompletionItemKind.Value);
              it.detail = `${meta.name}${meta.datatype ? ` (${meta.datatype})` : ''}`;
              it.insertText = num;
              it.range     = replaceRange;                  // <-- overwrite numeric token
              it.sortText  = '0_' + String(num).padStart(3,'0');
              it.filterText = 'hl7-field'; 
              it.preselect  = (currentNum === num);         // optional: highlight current number
              items.push(it);
            });
          return items;
        }
        
        // Fallback: try to grab seg from the visible prefix even if we didn't detect it above
        if (!segFromFirstArg && effIndex === 2) {
          const m = segArgText.match(/^["'`]([A-Za-z]{3})/);
          const guess = m ? m[1].toUpperCase() : undefined;
          if (guess && segments[guess]) {            
            const replaceRange = findNumericTokenReplaceRange(doc, pos);
            const fields = segments[guess] ?? {};
            Object.entries(fields)
              .sort(([a], [b]) => Number(a) - Number(b))
              .forEach(([num, meta]) => {
                const it = new vscode.CompletionItem(`${num}`, vscode.CompletionItemKind.Value);
                it.detail = `${meta.name}${meta.datatype ? ` (${meta.datatype})` : ''}`;
                it.insertText = num;
                it.filterText = num;
                it.sortText = '0_' + String(num).padStart(3, '0');
                it.range    = replaceRange;                 // overwrite numeric token here too
                items.push(it);
              });

            return items;
          }
        }

        // Arg 4 (component) suggestions if we can infer seg+field+datatype from the visible text
        if (segFromFirstArg && effIndex === 4) {
          const parts = readPartialArgs(open.textUpToCursor).args;
          // field is args[2] for member, args[3] for wrapper
          const fieldPart = isWrapper ? (parts[3] || '') : (parts[2] || '');
          const fieldGuess = fieldPart.replace(/[^\d]/g, '');
          const dtypeGuess = fieldGuess ? (segments[segFromFirstArg]?.[fieldGuess]?.datatype) : undefined;
          if (dtypeGuess) {
            const replaceRange = findNumericTokenReplaceRange(doc, pos);
            const currentNum = doc.getText(replaceRange).trim();
            const dt = dtypes[dtypeGuess] || {};

            const items: vscode.CompletionItem[] = [];
            Object.entries(dt)
              .filter(([k]) => /^\d+$/.test(k))
              .sort(([a], [b]) => Number(a) - Number(b))
              .forEach(([num, name]) => {
                const it = new vscode.CompletionItem(`${num}`, vscode.CompletionItemKind.Value);
                it.detail = String(name);
                it.insertText = num;
                it.range    = replaceRange;                 // <-- overwrite numeric token
                it.sortText = '0_' + String(num).padStart(3, '0');
                it.filterText = 'hl7-comp';
                it.preselect  = (currentNum === num);
                items.push(it);
              });
            return items;
          }
        }

        // Fall through → no special list
        return undefined;
      }

      // 2) If we couldn't find an open call, try the strict (balanced) parse (e.g., after you closed ')')
      const slice = findEnclosingCallAroundPosition(doc, pos, FN_CALL_RE, WRAP_FN_CALL_RE);
      if (!slice) return;
      const parsed = parseCallSignature(slice.text);
      if (!parsed) return;

      const { seg, field } = parsed;
      const segFields = seg ? segments[seg] ?? {} : {};
      const dtype = field ? segFields[field]?.datatype : undefined;

      const items: vscode.CompletionItem[] = [];
      const add = (label: string, detail?: string, insert?: string) => {
        const it = new vscode.CompletionItem(label, vscode.CompletionItemKind.Value);
        if (detail) it.detail = detail;
        if (insert) it.insertText = insert;
        it.sortText = '0_' + label;
        items.push(it);
      };

      // Determine arg index by commas (balanced slice)
      const ps = slice.text.indexOf('(');
      const pe = slice.text.lastIndexOf(')');
      let argIndex = 0;
      for (let i = ps + 1; i < pe; i++) if (slice.text[i] === ',') argIndex++;

      // Detect wrapper by peeking the first argument inside ()
      let wrapperOffset = 0;
      {
        const inner = slice.text.slice(ps + 1, pe).trim();
        const firstComma = inner.indexOf(',');
        const first = (firstComma === -1 ? inner : inner.slice(0, firstComma)).trim();
        if (first && IDENT_RE.test(first)) wrapperOffset = 1;
      }
      const effIndex = argIndex - wrapperOffset;

      if (effIndex === 0) {
        Object.keys(segments).forEach(s => add(`"${s}"`, 'Segment', `"${s}"`));
        return items;
      }
      if (effIndex === 2 && seg) {
        const replaceRange = findNumericTokenReplaceRange(doc, pos);
        const fields = segments[seg] ?? {};
        Object.entries(fields)
          .sort(([a],[b]) => Number(a) - Number(b))
          .forEach(([num, meta]) => {
            const it = new vscode.CompletionItem(`${num}`, vscode.CompletionItemKind.Value);
            it.detail = `${meta.name}${meta.datatype ? ` (${meta.datatype})` : ''}`;
            it.insertText = num;
            it.filterText = num;
            it.sortText  = '0_' + String(num).padStart(3,'0');
            it.range     = replaceRange;                  // overwrite numeric token
            items.push(it);
          });
        return items;
      }
      if (effIndex === 4 && dtype) {
        const replaceRange = findNumericTokenReplaceRange(doc, pos);
        const dt = dtypes[dtype] || {};
        Object.entries(dt)
          .filter(([k]) => /^\d+$/.test(k))
          .sort(([a],[b]) => Number(a) - Number(b))
          .forEach(([num, name]) => {
            const it = new vscode.CompletionItem(`${num}`, vscode.CompletionItemKind.Value);
            it.detail = String(name);
            it.insertText = num;
            it.filterText = num;
            it.sortText  = '0_' + String(num).padStart(3,'0');
            it.range     = replaceRange;                  // overwrite numeric token
            items.push(it);
          });
        return items;
      }

      return undefined;
    }
  };

  // --- Auto-trigger suggestions while typing inside the first arg (segment) or field/component slots
  const triggerSuggestIfHL7Context = debounce(() => {
    const ed = vscode.window.activeTextEditor;

    if (!ed) return;

    const doc = ed.document;
    const pos = ed.selection.active;

    // Only run for our languages
    const lang = doc.languageId;
    if (!['javascript', 'typescript', 'javascriptreact', 'typescriptreact'].includes(lang)) return;

    // Reuse our tolerant open-call detector
    const open = findOpenCallPrefixAroundPosition(doc, pos, FN_CALL_RE, WRAP_FN_CALL_RE);
    if (!open) return;

    const { argIndex, firstArgText } = readPartialArgs(open.textUpToCursor);

    // Wrapper?
    const isWrapper = !!firstArgText && IDENT_RE.test(firstArgText);
    const effIndex = isWrapper ? argIndex - 1 : argIndex;

    // Are we clearly in a place where our completions are meaningful?
    // - effIndex === 0  → segment name (inside quotes)
    // - effIndex === 2  → field
    // - effIndex === 4  → component
    if (effIndex === 0 || effIndex === 2 || effIndex === 4) {
      // Fire the suggest widget (safe to call; it's debounced)
      vscode.commands.executeCommand('editor.action.triggerSuggest');
    }
  }, 80);

  // Listen to user typing; only react to small inserts (single chars)
  context.subscriptions.push(
    vscode.workspace.onDidChangeTextDocument(e => {
      // Ignore if not the active editor
      const ed = vscode.window.activeTextEditor;
      if (!ed || e.document !== ed.document) return;

      // Look for small, insert-type changes that usually correspond to typing
      for (const ch of e.contentChanges) {
        if (ch.text.length === 1 || ch.text === '"' || ch.text === "'" || ch.text === '`') {
          triggerSuggestIfHL7Context();
          break;
        }
      }
    })
  );

  context.subscriptions.push(
    vscode.languages.registerCompletionItemProvider(
      [
        { language: 'javascript' }, { language: 'typescript' },
        { language: 'javascriptreact' }, { language: 'typescriptreact' }
      ],
      completionProvider,
      // trigger on quotes, slash (path), dash/dot/commas/numbers, AND letters (for "PID" as you type)
      '"', "'", '/', '-', '.', ',', 
      '0','1','2','3','4','5','6','7','8','9',
      'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
      'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'
    )
  );

  /* ---------- Commands ---------- */
  context.subscriptions.push(
    vscode.commands.registerCommand('hl7Helper.openSegmentSpec', (seg: string) => {
      const target = `${specBaseUrl}/${versionPath}/Segments/${seg}`;
      vscode.env.openExternal(vscode.Uri.parse(target));
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('hl7Helper.openDatatypeSpec', (dtype: string) => {
      const target = `${specBaseUrl}/${versionPath}/DataTypes/${dtype}`;
      vscode.env.openExternal(vscode.Uri.parse(target));
    })
  );
}

/* =========================
 * Deactivate
 * ========================= */
export function deactivate() {}
