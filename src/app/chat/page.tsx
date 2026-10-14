import Chat from "@/components/Chat";
import { listAgents } from "@/lib/agents";

export const dynamic = "force-dynamic";
export const metadata = { title: "Assistant" };

export default async function ChatPage({ searchParams }: { searchParams: Promise<{ c?: string }> }) {
  const { c } = await searchParams;
  const agents = (await listAgents()).map((a) => ({ id: String(a._id), name: a.name, module: a.module }));
  return <Chat agents={agents} initialConversation={c} />;
}
