import { readdirSync, readFileSync, statSync } from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { maskForLlm } from "@/lib/llm";

function files(dir: string): string[] {
  return readdirSync(dir).flatMap((f) => {
    const p = path.join(dir, f);
    return statSync(p).isDirectory() ? files(p) : /\.(ts|tsx)$/.test(f) ? [p] : [];
  });
}

describe("LLM gateway", () => {
  it("only src/lib/llm.ts talks to Claude (so every prompt is masked)", () => {
    const offenders = files(path.resolve(__dirname, "../src"))
      .filter((f) => !f.endsWith(path.join("lib", "llm.ts")))
      .filter((f) => /messages\.(create|stream)\s*\(|new Anthropic\s*\(|from ["']@anthropic-ai\/sdk["'](?!.*type)/.test(readFileSync(f, "utf8").replace(/import type[^;]+;/g, "")));
    expect(offenders).toEqual([]);
  });

  it("masks system, user text and tool results; leaves Claude's own turns", () => {
    const m = maskForLlm("Owner jane.client@gmail.com", [
      { role: "user", content: "ssn 123-45-6789 please" },
      { role: "assistant", content: [{ type: "tool_use", id: "t1", name: "get_incident", input: { incident: "INC0099213" } }] },
      { role: "user", content: [{ type: "tool_result", tool_use_id: "t1", content: "card 4111 1111 1111 1111" }] },
    ]);
    expect(m.system).toBe("Owner [REDACTED-EMAIL]");
    expect(m.messages[0].content).toBe("ssn [REDACTED-SSN] please");
    expect(m.messages[1]).toEqual({ role: "assistant", content: [{ type: "tool_use", id: "t1", name: "get_incident", input: { incident: "INC0099213" } }] });
    expect(JSON.stringify(m.messages[2])).toContain("[REDACTED-CARD …1111]");
    expect(m.counts).toEqual({ EMAIL: 1, SSN: 1, CARD: 1 });
  });
});
