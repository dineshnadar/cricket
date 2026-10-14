import { describe, expect, it } from "vitest";
import { TOOLS, toolDefinitions } from "@/lib/assistant/tools";

describe("assistant tools", () => {
  it("have unique names and object schemas", () => {
    const names = TOOLS.map((t) => t.name);
    expect(new Set(names).size).toBe(names.length);
    for (const t of toolDefinitions()) expect((t.input_schema as { type: string }).type).toBe("object");
  });
  it("flags every write tool as needing confirmation", () => {
    const defs = new Map(toolDefinitions().map((d) => [d.name, d.description]));
    for (const t of TOOLS.filter((x) => x.kind === "write")) expect(defs.get(t.name)).toContain("must confirm");
    expect(TOOLS.filter((x) => x.kind === "write").map((t) => t.name)).toEqual(
      expect.arrayContaining(["trigger_pipeline", "create_agent", "update_agent", "open_setup_prs"]),
    );
  });
});
