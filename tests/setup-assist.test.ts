import { describe, expect, it } from "vitest";
import { fieldQuality } from "@/lib/field-quality";
import { checkGlobs, summariseTree } from "@/lib/setup-assist";

const TREE = [
  "README.md",
  "docs/architecture.md",
  "src/main/java/com/acme/poa/PoaValidator.java",
  "src/main/java/com/acme/poa/ExpiryService.java",
  "src/main/java/com/acme/common/HttpClient.java",
  "src/main/resources/poa-rules.yml",
  "src/test/java/com/acme/poa/PoaValidatorTest.java",
];

describe("field quality hint", () => {
  it("flags empty and vague module scopes", () => {
    expect(fieldQuality("module", "").level).toBe("empty");
    expect(fieldQuality("module", "poa card cip stuff").level).toBe("vague");
  });
  it("asks for out-of-scope, then rates a full scope as good", () => {
    const noScope = "POA setup and validation, check deposit and processing, card issuance and limits, CIP identity verification";
    expect(fieldQuality("module", noScope).tip).toMatch(/out of scope/i);
    const full =
      "In scope: POA setup and validation; check deposit and processing; card issuance and limits; CIP identity verification for advisory accounts. Out of scope: statements, marketing pages, reporting. Critical flows: POA expiry checks.";
    expect(fieldQuality("module", full).level).toBe("good");
  });
  it("wants code names and an owner in repo notes", () => {
    expect(fieldQuality("repoNotes", "poa stuff").level).toBe("vague");
    expect(fieldQuality("repoNotes", "Handles the power of attorney service and its rules for advisors").tip).toMatch(/classes|endpoints/);
    expect(fieldQuality("repoNotes", "Validates powers of attorney. Critical code: PoaValidator, /api/poa/ endpoints").tip).toMatch(/owning team/);
    expect(fieldQuality("repoNotes", "Validates powers of attorney. Critical code: PoaValidator, /api/poa/ endpoints. Owner: POA team").level).toBe("good");
  });
});

describe("suggested paths are checked against the real tree", () => {
  it("drops includes that match nothing and counts the rest", () => {
    const r = checkGlobs(["src/main/java/**/poa/**", "src/test/java/**/poa/**", "src/**/legacy/**"], ["**/*.md"], TREE);
    expect(r.dropped).toEqual(["src/**/legacy/**"]);
    expect(r.paths).toEqual(["src/main/java/**/poa/**", "src/test/java/**/poa/**", "!**/*.md"]);
    expect(r.counts).toContainEqual({ glob: "src/main/java/**/poa/**", matches: 2 });
    expect(r.counts).toContainEqual({ glob: "!**/*.md", matches: 2 });
    expect(r.matched).toBe(3);
  });
  it("accepts excludes written with or without the leading !", () => {
    const r = checkGlobs(["**"], ["!docs/**", "**/*.md"], TREE);
    expect(r.paths).toEqual(["**", "!docs/**", "!**/*.md"]);
    expect(r.matched).toBe(5);
  });
});

describe("tree summary", () => {
  it("lists folders with file counts and sample source files", () => {
    const s = summariseTree(TREE);
    expect(s).toContain("src/main/java/com/acme/poa/ (2 files)");
    expect(s).toContain("src/main/resources/poa-rules.yml");
    expect(s).not.toContain("README.md\n"); // only source/config in the sample
  });
});
