import { collections } from "@/lib/db";
import { integrationStatus } from "@/lib/env";
import { errMsg } from "@/lib/util";

export const dynamic = "force-dynamic";

export async function GET() {
  try {
    const { meta } = await collections();
    const hb = await meta.findOne({ _id: "worker:heartbeat" });
    const workerAt = hb ? new Date(hb.value as string) : null;
    return Response.json({
      ok: true,
      db: true,
      worker: workerAt ? { lastSeen: workerAt, alive: Date.now() - workerAt.getTime() < 120_000 } : null,
      integrations: integrationStatus(),
    });
  } catch (e) {
    return Response.json({ ok: false, error: errMsg(e) }, { status: 503 });
  }
}
