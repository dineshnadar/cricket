import { describe, expect, it } from "vitest";
import { luhn, redact, redactDeep, redactWithCounts } from "@/lib/redact";

describe("redact: what gets masked", () => {
  const cases: [string, string, RegExp][] = [
    ["SSN dashed", "client ssn 123-45-6789 failed", /\[REDACTED-SSN\]/],
    ["SSN spaced", "SSN 123 45 6789", /\[REDACTED-SSN\]/],
    ["SSN bare with keyword", "social security number: 123456789", /\[REDACTED-SSN\]/],
    ["TIN", "tax id 987654321", /\[REDACTED-SSN\]/],
    ["EIN", "EIN 12-3456789", /\[REDACTED-EIN\]/],
    ["card (Luhn)", "card 4111 1111 1111 1111 declined", /\[REDACTED-CARD …1111\]/],
    ["card no spaces", "pan=5555555555554444", /\[REDACTED-CARD …4444\]/],
    ["account named", "acct 000123456789", /\[REDACTED-ACCT …6789\]/],
    ["short account named", "account: 12345678", /\[REDACTED-ACCT …5678\]/],
    ["routing", "routing 021000021", /\[REDACTED-ACCT …0021\]/],
    ["long id in logs", "clientId=98765432101 not found", /\[REDACTED-NUM …2101\]/],
    ["DOB", "DOB: 01/02/1980", /\[REDACTED-DOB\]/],
    ["phone", "call (212) 555-0147 today", /\[REDACTED-PHONE\]/],
    ["email (client)", "sent to jane.client@gmail.com", /\[REDACTED-EMAIL\]/],
    ["secret", "password=Sup3rS3cretValue", /password=\[REDACTED-SECRET\]/],
    ["bearer", "Authorization: Bearer abcdef1234567890", /Bearer \[REDACTED-SECRET\]/],
    ["JWT", "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U", /\[REDACTED-JWT\]/],
    ["AWS key", "key AKIAIOSFODNN7EXAMPLE", /\[REDACTED-AWS-KEY\]/],
    ["private key", "-----BEGIN RSA PRIVATE KEY-----\nMIIBOg\n-----END RSA PRIVATE KEY-----", /\[REDACTED-PRIVATE-KEY\]/],
  ];
  for (const [name, input, expected] of cases) {
    it(name, () => {
      const out = redact(input);
      expect(out).toMatch(expected);
    });
  }

  it("never leaves the raw value behind", () => {
    const raw = ["123-45-6789", "4111 1111 1111 1111", "000123456789", "01/02/1980", "jane.client@gmail.com", "Sup3rS3cretValue"];
    const out = redact(`ssn 123-45-6789 card 4111 1111 1111 1111 acct 000123456789 DOB 01/02/1980 jane.client@gmail.com password: Sup3rS3cretValue`);
    for (const r of raw) expect(out).not.toContain(r);
  });
});

describe("redact: what stays readable", () => {
  it("leaves code, shas, dates, versions and short numbers alone", () => {
    const code = "if (retries > 3) return a1b2c3d4e5f6a7b8c9d0; // since 2026-10-13 v1.12.3 port 8089 build #412";
    expect(redact(code, { emails: false })).toBe(code);
  });
  it("doesn't treat random long digit runs as cards (Luhn fails) but still masks them as numbers", () => {
    expect(luhn("4111111111111111")).toBe(true);
    expect(luhn("1234567890123456")).toBe(false);
    expect(redact("id 1234567890123456")).toBe("id [REDACTED-NUM …3456]");
  });
  it("keeps internal email domains readable when allow-listed", () => {
    expect(redact("notify cards-team@acme.com and x@sub.acme.com", { allowEmailDomains: ["acme.com"] })).toBe(
      "notify cards-team@acme.com and x@sub.acme.com",
    );
    expect(redact("from client@gmail.com", { allowEmailDomains: ["acme.com"] })).toBe("from [REDACTED-EMAIL]");
  });
  it("is idempotent", () => {
    const once = redact("ssn 123-45-6789 acct 000123456789 card 4111111111111111");
    expect(redact(once)).toBe(once);
  });
});

describe("redact: custom patterns and counts", () => {
  it("applies organisation-specific patterns first", () => {
    const out = redact("client CL12345678 opened", { extraPatterns: [{ name: "client id", pattern: "\\bCL\\d{8}\\b" }] });
    expect(out).toBe("client [REDACTED-CLIENT_ID] opened");
  });
  it("counts what it masked", () => {
    const { counts } = redactWithCounts("ssn 123-45-6789 and 987-65-4321, card 4111111111111111");
    expect(counts).toEqual({ SSN: 2, CARD: 1 });
  });
  it("redacts nested values", () => {
    expect(redactDeep({ a: ["x 123-45-6789"], n: 5 })).toEqual({ a: ["x [REDACTED-SSN]"], n: 5 });
  });
});
