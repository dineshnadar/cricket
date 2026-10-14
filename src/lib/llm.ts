import Anthropic from "@anthropic-ai/sdk";
import type { z } from "zod";
import { env } from "./env";
import { describeCounts, redactWithCounts, type RedactionCounts } from "./redact";

/**
 * The ONLY module allowed to talk to Claude (tests/llm-gateway.test.ts fails the
 * build if any other file calls messages.create / messages.stream).
 * Every outgoing system prompt, user message and tool result is masked here.
 */

let _client: Anthropic | null = null;

function client(): Anthropic {
  if (!env.anthropicApiKey) throw new Error("ANTHROPIC_API_KEY is not set");
  if (!_client) {
    _client = new Anthropic({
      apiKey: env.anthropicApiKey,
      ...(env.anthropicBaseUrl ? { baseURL: env.anthropicBaseUrl } : {}),
      maxRetries: 3,
      timeout: 120_000,
    });
  }
  return _client;
}

// ---------- masking ----------

function add(into: RedactionCounts, from: RedactionCounts) {
  for (const [k, v] of Object.entries(from)) into[k] = (into[k] ?? 0) + v;
}

function maskString(s: string, counts: RedactionCounts): string {
  const r = redactWithCounts(s);
  add(counts, r.counts);
  return r.text;
}

/** Mask one message's content. Assistant turns are Claude's own output (already derived from masked input). */
function maskMessage(m: Anthropic.MessageParam, counts: RedactionCounts): Anthropic.MessageParam {
  if (m.role !== "user") return m;
  if (typeof m.content === "string") return { ...m, content: maskString(m.content, counts) };
  return {
    ...m,
    content: m.content.map((block) => {
      if (block.type === "text") return { ...block, text: maskString(block.text, counts) };
      if (block.type === "tool_result") {
        if (typeof block.content === "string") return { ...block, content: maskString(block.content, counts) };
        if (Array.isArray(block.content)) {
          return {
            ...block,
            content: block.content.map((c) => (c.type === "text" ? { ...c, text: maskString(c.text, counts) } : c)),
          };
        }
      }
      return block;
    }),
  };
}

export interface Masked {
  system: string;
  messages: Anthropic.MessageParam[];
  counts: RedactionCounts;
}

/** Exported for tests and for showing users what was masked. */
export function maskForLlm(system: string, messages: Anthropic.MessageParam[]): Masked {
  const counts: RedactionCounts = {};
  return { system: maskString(system, counts), messages: messages.map((m) => maskMessage(m, counts)), counts };
}

function logMasking(purpose: string, counts: RedactionCounts) {
  const d = describeCounts(counts);
  if (d) console.info(`[redact] ${purpose}: masked ${d}`);
}

// ---------- calls ----------

/**
 * Ask Claude for a structured result by forcing a single tool call, then
 * validate the tool input with zod.
 */
export async function structured<T>(opts: {
  purpose: string;
  system: string;
  prompt: string;
  toolName: string;
  toolDescription: string;
  inputSchema: Record<string, unknown>;
  validator: z.ZodType<T, z.ZodTypeDef, unknown>;
  maxTokens?: number;
}): Promise<{ data: T; model: string; masked: RedactionCounts }> {
  const m = maskForLlm(opts.system, [{ role: "user", content: opts.prompt }]);
  logMasking(opts.purpose, m.counts);
  const res = await client().messages.create({
    model: env.anthropicModel,
    max_tokens: opts.maxTokens ?? 2000,
    system: m.system,
    messages: m.messages,
    tools: [
      {
        name: opts.toolName,
        description: opts.toolDescription,
        input_schema: opts.inputSchema as Anthropic.Tool.InputSchema,
      },
    ],
    tool_choice: { type: "tool", name: opts.toolName },
  });
  const block = res.content.find((b) => b.type === "tool_use");
  if (!block || block.type !== "tool_use") {
    throw new Error(`Wisdom AI did not return a ${opts.toolName} result (stop_reason=${res.stop_reason})`);
  }
  const parsed = opts.validator.safeParse(block.input);
  if (!parsed.success) {
    throw new Error(`Wisdom AI ${opts.toolName} output failed validation: ${parsed.error.message}`);
  }
  return { data: parsed.data, model: res.model, masked: m.counts };
}

export async function freeText(opts: { purpose: string; system: string; prompt: string; maxTokens?: number }): Promise<string> {
  const m = maskForLlm(opts.system, [{ role: "user", content: opts.prompt }]);
  logMasking(opts.purpose, m.counts);
  const res = await client().messages.create({
    model: env.anthropicModel,
    max_tokens: opts.maxTokens ?? 2500,
    system: m.system,
    messages: m.messages,
  });
  return res.content
    .filter((b): b is Anthropic.TextBlock => b.type === "text")
    .map((b) => b.text)
    .join("\n")
    .trim();
}

/** Streaming chat turn with tools (Wisdom Assistant). Masks the whole conversation before sending. */
export function chatStream(opts: {
  purpose: string;
  system: string;
  messages: Anthropic.MessageParam[];
  tools: Anthropic.Tool[];
  maxTokens?: number;
}) {
  const m = maskForLlm(opts.system, opts.messages);
  logMasking(opts.purpose, m.counts);
  const stream = client().messages.stream({
    model: env.anthropicModel,
    max_tokens: opts.maxTokens ?? 2500,
    system: m.system,
    tools: opts.tools,
    messages: m.messages,
  });
  return { stream, masked: m.counts };
}
