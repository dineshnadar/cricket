/**
 * Citation check for Assistant answers. Claude cites records as [change:<id>],
 * [incident:<id>] and [agent:<id>]. A citation is kept only if that id actually
 * appeared in something Claude was shown this turn (tool results, the system
 * prompt) or earlier in the conversation. Anything else is a made-up link and is removed.
 */

const CITE = /\s?\[(change|incident|agent):([a-f0-9]{24})\]/g;
const OBJECT_ID = /[a-f0-9]{24}/g;

/** Every 24-hex id found in the given texts. */
export function collectIds(texts: Iterable<string>, into: Set<string> = new Set()): Set<string> {
  for (const t of texts) for (const m of t.matchAll(OBJECT_ID)) into.add(m[0]);
  return into;
}

export function dropUncitedLinks(text: string, known: Set<string>): { text: string; dropped: string[] } {
  const dropped: string[] = [];
  const out = text.replace(CITE, (m, kind: string, id: string) => {
    if (known.has(id)) return m;
    dropped.push(`${kind}:${id}`);
    return "";
  });
  if (!dropped.length) return { text, dropped };
  const n = dropped.length;
  const note = `\n\n_Removed ${n} link${n > 1 ? "s" : ""} to ${n > 1 ? "records" : "a record"} I hadn't actually looked up; treat the related statement${n > 1 ? "s" : ""} with care._`;
  return { text: out + note, dropped };
}
