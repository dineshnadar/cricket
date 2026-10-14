import Link from "next/link";
import { ChevronRight } from "lucide-react";
import AgentForm from "@/components/AgentForm";

export const metadata = { title: "Create agent" };

export default function NewAgentPage() {
  return (
    <>
      <div className="page-head">
        <div>
          <nav className="crumbs" aria-label="Breadcrumb">
            <Link href="/agents">Agents</Link>
            <ChevronRight size={14} aria-hidden />
            <span>New</span>
          </nav>
          <h1>Create agent</h1>
          <div className="sub">Pick the repos behind a module, tell Wisdom AI what belongs to it, and wire up tests and alerts.</div>
        </div>
      </div>
      <AgentForm />
    </>
  );
}
