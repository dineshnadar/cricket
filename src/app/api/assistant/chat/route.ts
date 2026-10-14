import { z } from "zod";
import { runChat } from "@/lib/assistant/loop";
import { requestUser } from "@/lib/assistant/user";
import { toId } from "@/lib/db";
import { fail } from "@/lib/http";
import { formatZodError } from "@/lib/schemas";

export const dynamic = "force-dynamic";
export const maxDuration = 300;

const body = z.object({
  message: z.string().trim().min(1).max(8000),
  conversationId: z.string().optional(),
  agentId: z.string().optional(),
});

/** Streams newline-delimited JSON events (see ChatEvent in lib/assistant/loop.ts). */
export async function POST(req: Request) {
  const parsed = body.safeParse(await req.json().catch(() => null));
  if (!parsed.success) return fail(formatZodError(parsed.error));
  const user = requestUser(req);
  const { message, conversationId, agentId } = parsed.data;

  const gen = runChat({
    message,
    user,
    conversationId: conversationId ? (toId(conversationId) ?? undefined) : undefined,
    agentId: agentId ? (toId(agentId) ?? undefined) : undefined,
  });
  const enc = new TextEncoder();
  const stream = new ReadableStream<Uint8Array>({
    async pull(controller) {
      try {
        const { value, done } = await gen.next();
        if (done) return controller.close();
        controller.enqueue(enc.encode(JSON.stringify(value) + "\n"));
      } catch (e) {
        controller.enqueue(enc.encode(JSON.stringify({ type: "error", message: String(e) }) + "\n"));
        controller.close();
      }
    },
    async cancel() {
      await gen.return(undefined);
    },
  });
  return new Response(stream, {
    headers: {
      "Content-Type": "application/x-ndjson; charset=utf-8",
      "Cache-Control": "no-cache, no-transform",
      "X-Accel-Buffering": "no",
    },
  });
}
