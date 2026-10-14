import Link from "next/link";
import { SearchX } from "lucide-react";

export default function NotFound() {
  return (
    <section className="panel" style={{ maxWidth: 520 }}>
      <h2 className="row" style={{ gap: 8 }}>
        <SearchX size={18} aria-hidden className="icon-muted" /> Not found
      </h2>
      <p>That page doesn&apos;t exist or was deleted.</p>
      <Link className="btn" href="/">
        Back to dashboard
      </Link>
    </section>
  );
}
