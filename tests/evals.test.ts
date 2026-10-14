import { describe, expect, it } from "vitest";
import { acted, incidentMetrics, incidentReport, triageMetrics, triageReport, type IncidentRow, type TriageRow } from "@/lib/evals";

const row = (expectedRelevant: boolean, relevant: boolean, confidence: number, extra: Partial<TriageRow> = {}): TriageRow => ({
  changeId: Math.random().toString(16).slice(2).padEnd(24, "0").slice(0, 24),
  repo: "acme/poa",
  sha: "abc1234",
  expectedRelevant,
  relevant,
  confidence,
  threshold: 0.6,
  ...extra,
});

describe("triage metrics", () => {
  it("counts what the system acted on, so a relevant answer below the threshold is a miss", () => {
    expect(acted({ relevant: true, confidence: 0.59, threshold: 0.6 })).toBe(false);
    expect(acted({ relevant: true, confidence: 0.6, threshold: 0.6 })).toBe(true);
    const m = triageMetrics([
      row(true, true, 0.9), // TP
      row(true, true, 0.5), // FN: relevant but under threshold
      row(true, false, 0.8), // FN
      row(false, true, 0.7), // FP
      row(false, false, 0.9), // TN
    ]);
    expect([m.tp, m.fn, m.fp, m.tn]).toEqual([1, 2, 1, 1]);
    expect(m.recall).toBeCloseTo(1 / 3);
    expect(m.precision).toBeCloseTo(1 / 2);
    expect(m.accuracy).toBeCloseTo(2 / 5);
    expect(m.falseNegatives).toHaveLength(2);
  });

  it("measures risk agreement only where a risk was labelled, and skips errored rows", () => {
    const m = triageMetrics([
      row(true, true, 0.9, { expectedRisk: "high", riskLevel: "high" }),
      row(true, true, 0.9, { expectedRisk: "high", riskLevel: "low" }),
      row(true, true, 0.9),
      row(true, false, 0, { error: "boom" }),
    ]);
    expect(m.riskAgreement).toBe(0.5);
    expect(m.riskLabelled).toBe(2);
    expect(m.n).toBe(3);
    expect(m.errors).toBe(1);
  });

  it("returns n/a rather than dividing by zero", () => {
    const m = triageMetrics([row(false, false, 0.9)]);
    expect(m.recall).toBeNull();
    expect(triageReport(m)).toContain("n/a");
  });
});

describe("incident metrics", () => {
  const A = "a".repeat(24);
  const B = "b".repeat(24);
  const C = "c".repeat(24);
  const inc = (expectedCause: string | null, causes: IncidentRow["causes"]): IncidentRow => ({ incidentId: "x".repeat(24), number: "INC1", expectedCause, causes });

  it("scores the true cause at rank 1 and within the top 3", () => {
    const m = incidentMetrics([
      inc(A, [{ changeId: A, confidence: 0.8 }]),
      inc(A, [{ changeId: B, confidence: 0.7 }, { changeId: A, confidence: 0.5 }]),
      inc(A, [{ changeId: B, confidence: 0.7 }, { changeId: C, confidence: 0.5 }]),
    ]);
    expect(m.hitAt1).toBeCloseTo(1 / 3);
    expect(m.hitAt3).toBeCloseTo(2 / 3);
    expect(m.misses).toHaveLength(1);
  });

  it("rewards not blaming a change when the incident wasn't caused by code", () => {
    const m = incidentMetrics([
      inc(null, []),
      inc(null, [{ changeId: A, confidence: 0.2 }]),
      inc(null, [{ changeId: A, confidence: 0.9 }]),
    ]);
    expect(m.noCauseCorrect).toBeCloseTo(2 / 3);
    expect(incidentReport(m)).toContain("no code change");
  });
});
