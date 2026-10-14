"use client";

import { useCallback, useEffect, useRef, useState } from "react";
import Markdown from "react-markdown";
import {
  ArrowUp,
  Bot,
  Check,
  CircleAlert,
  CircleCheck,
  CircleX,
  LoaderCircle,
  MessageSquarePlus,
  Search,
  ShieldCheck,
  Sparkles,
  Square,
  Trash2,
  UserRound,
  X,
} from "lucide-react";
import type { DisplayMessage, PendingActionView, ToolActivity } from "@/lib/types";
import { CopyBox } from "./client";

interface AgentOption {
  id: string;
  name: string;
  module: string;
}
interface ConvSummary {
  id: string;
  title: string;
  updatedAt: string;
}
type Msg = Omit<DisplayMessage, "at"> & { at?: string | Date; streaming?: boolean; maskedNote?: string };

const SUGGESTIONS = [
  "What changed in the last 7 days, and what's risky?",
  "Why did the last regression run fail?",
  "Summarise the latest production incident and its likely cause",
  "Set up a new agent for a module",
];

const store = {
  get(k: string) {
    try {
      return localStorage.getItem(k) ?? "";
    } catch {
      return "";
    }
  },
  set(k: string, v: string) {
    try {
      localStorage.setItem(k, v);
    } catch {
      /* unavailable */
    }
  },
};

function linkify(s: string) {
  return s
    .replace(/\[change:([a-f0-9]{24})\]/g, "[change ↗](/changes/$1)")
    .replace(/\[incident:([a-f0-9]{24})\]/g, "[incident ↗](/incidents/$1)")
    .replace(/\[agent:([a-f0-9]{24})\]/g, "[agent ↗](/agents/$1)");
}

function NamePrompt({ onSave }: { onSave: (n: string) => void }) {
  const [name, setName] = useState("");
  return (
    <div className="chat-empty">
      <form
        className="panel"
        style={{ maxWidth: 440, width: "100%" }}
        onSubmit={(e) => {
          e.preventDefault();
          if (name.trim()) onSave(name.trim());
        }}
      >
        <h2 className="row" style={{ gap: 8 }}>
          <UserRound size={18} aria-hidden className="icon-muted" /> Who&apos;s asking?
        </h2>
        <div className="field">
          <label htmlFor="wo-name">Your name or user id</label>
          <input id="wo-name" autoFocus value={name} onChange={(e) => setName(e.target.value)} placeholder="e.g. dinesh.n" />
          <div className="hint">Recorded with each question and action for the audit log. There&apos;s no sign-in yet.</div>
        </div>
        <button className="btn primary" disabled={!name.trim()}>
          Continue
        </button>
      </form>
    </div>
  );
}

function ToolTrail({ tools, streaming }: { tools: ToolActivity[]; streaming?: boolean }) {
  if (!tools.length) return null;
  const running = tools.some((t) => t.ok === undefined);
  return (
    <details className="tool-trail" open={streaming && running}>
      <summary>
        {running ? <LoaderCircle size={14} className="spin" aria-hidden /> : <Search size={14} aria-hidden />}
        {running ? "Looking things up…" : `Checked ${tools.length} source${tools.length === 1 ? "" : "s"}`}
      </summary>
      <ul>
        {tools.map((t) => (
          <li key={t.id}>
            {t.ok === undefined ? (
              <LoaderCircle size={13} className="spin" aria-hidden />
            ) : t.ok ? (
              <CircleCheck size={13} aria-hidden style={{ color: "var(--color-good)" }} />
            ) : (
              <CircleX size={13} aria-hidden style={{ color: "var(--color-bad)" }} />
            )}
            <span>{t.label}</span>
            {t.summary && <span className="cell-sub" style={{ margin: 0 }}>· {t.summary}</span>}
          </li>
        ))}
      </ul>
    </details>
  );
}

function ActionCard({ action, user, onUpdate }: { action: PendingActionView; user: string; onUpdate: (a: PendingActionView) => void }) {
  const [busy, setBusy] = useState<"" | "confirm" | "cancel">("");
  const [err, setErr] = useState("");
  const decide = async (decision: "confirm" | "cancel") => {
    setBusy(decision);
    setErr("");
    try {
      const res = await fetch(`/api/assistant/actions/${action.id}`, {
        method: "POST",
        headers: { "Content-Type": "application/json", "x-wisdom-user": user },
        body: JSON.stringify({ decision }),
      });
      const j = await res.json();
      if (!res.ok) throw new Error(j.error ?? `Failed (${res.status})`);
      onUpdate(j);
    } catch (e) {
      setErr(e instanceof Error ? e.message : String(e));
    } finally {
      setBusy("");
    }
  };
  const statusBadge =
    action.status === "pending" ? null : (
      <span className={`badge ${action.status === "confirmed" ? "b-good" : action.status === "failed" ? "b-bad" : "b-muted"}`}>
        {action.status === "confirmed" ? <CircleCheck size={13} aria-hidden /> : action.status === "failed" ? <CircleX size={13} aria-hidden /> : <X size={13} aria-hidden />}
        {action.status === "confirmed" ? "done" : action.status}
      </span>
    );
  return (
    <div className={`action-card${action.status === "pending" ? " pending" : ""}`}>
      <div className="spread">
        <div className="row" style={{ gap: 8, fontWeight: 600 }}>
          <ShieldCheck size={16} aria-hidden className="icon-muted" />
          {action.title}
        </div>
        {statusBadge}
      </div>
      <ul className="action-details">
        {action.details.map((d, i) => (
          <li key={i}>{d}</li>
        ))}
      </ul>
      {action.status === "pending" && (
        <div className="row">
          <button className="btn primary small" onClick={() => decide("confirm")} disabled={!!busy}>
            {busy === "confirm" ? <LoaderCircle size={14} className="spin" aria-hidden /> : <Check size={14} aria-hidden />}
            Confirm
          </button>
          <button className="btn small ghost" onClick={() => decide("cancel")} disabled={!!busy}>
            Cancel
          </button>
          <span className="hint" style={{ margin: 0 }}>
            Nothing runs until you confirm.
          </span>
        </div>
      )}
      {action.resultText && <pre className="action-result">{action.resultText}</pre>}
      {action.secret && (
        <div style={{ marginTop: 8 }}>
          <div className="notice" style={{ marginBottom: 8 }}>
            <CircleAlert size={16} aria-hidden />
            <div>
              Ingest token: store it as the <code>WISDOM_TOKEN</code> secret now. It won&apos;t be shown again.
            </div>
          </div>
          <CopyBox text={action.secret} />
        </div>
      )}
      {err && (
        <div className="inline-error" role="alert">
          <CircleAlert size={14} aria-hidden /> {err}
        </div>
      )}
    </div>
  );
}

export default function Chat({ agents, initialConversation }: { agents: AgentOption[]; initialConversation?: string }) {
  const [user, setUser] = useState<string | null>(null);
  const [agentId, setAgentId] = useState("");
  const [convs, setConvs] = useState<ConvSummary[]>([]);
  const [convId, setConvId] = useState<string | undefined>(initialConversation);
  const [messages, setMessages] = useState<Msg[]>([]);
  const [input, setInput] = useState("");
  const [streaming, setStreaming] = useState(false);
  const [error, setError] = useState("");
  const abortRef = useRef<AbortController | null>(null);
  const endRef = useRef<HTMLDivElement | null>(null);
  const inputRef = useRef<HTMLTextAreaElement | null>(null);

  useEffect(() => {
    setUser(store.get("wo-user"));
    const saved = store.get("wo-agent");
    if (saved && agents.some((a) => a.id === saved)) setAgentId(saved);
    else if (agents.length === 1) setAgentId(agents[0].id);
  }, [agents]);

  const headers = useCallback((): HeadersInit => ({ "Content-Type": "application/json", "x-wisdom-user": user ?? "" }), [user]);

  const refreshConvs = useCallback(async () => {
    if (!user) return;
    const res = await fetch("/api/assistant/conversations", { headers: headers() });
    if (res.ok) setConvs(await res.json());
  }, [user, headers]);

  useEffect(() => {
    void refreshConvs();
  }, [refreshConvs]);

  const openConv = useCallback(
    async (id: string) => {
      if (streaming) return;
      setError("");
      const res = await fetch(`/api/assistant/conversations/${id}`, { headers: headers() });
      if (!res.ok) {
        setError("That conversation isn't available.");
        return;
      }
      const j = await res.json();
      setConvId(j.id);
      setMessages(j.messages);
      if (j.agentId) setAgentId(j.agentId);
      window.history.replaceState(null, "", `/chat?c=${j.id}`);
    },
    [headers, streaming],
  );

  useEffect(() => {
    if (user && initialConversation) void openConv(initialConversation);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [user]);

  useEffect(() => {
    endRef.current?.scrollIntoView({ block: "end" });
  }, [messages]);

  const newChat = () => {
    if (streaming) return;
    setConvId(undefined);
    setMessages([]);
    setError("");
    window.history.replaceState(null, "", "/chat");
    inputRef.current?.focus();
  };

  const patchLast = (fn: (m: Msg) => Msg) =>
    setMessages((ms) => {
      const copy = [...ms];
      copy[copy.length - 1] = fn(copy[copy.length - 1]);
      return copy;
    });

  const send = async (text: string) => {
    const message = text.trim();
    if (!message || streaming || !user) return;
    setInput("");
    setError("");
    setStreaming(true);
    setMessages((ms) => [...ms, { role: "user", text: message }, { role: "assistant", text: "", tools: [], actions: [], streaming: true }]);
    const ctrl = new AbortController();
    abortRef.current = ctrl;
    try {
      const res = await fetch("/api/assistant/chat", {
        method: "POST",
        headers: headers(),
        body: JSON.stringify({ message, conversationId: convId, agentId: agentId || undefined }),
        signal: ctrl.signal,
      });
      if (!res.ok || !res.body) {
        const j = await res.json().catch(() => ({}));
        throw new Error(j.error ?? `Request failed (${res.status})`);
      }
      const reader = res.body.getReader();
      const dec = new TextDecoder();
      let buf = "";
      for (;;) {
        const { value, done } = await reader.read();
        if (done) break;
        buf += dec.decode(value, { stream: true });
        let nl: number;
        while ((nl = buf.indexOf("\n")) >= 0) {
          const line = buf.slice(0, nl).trim();
          buf = buf.slice(nl + 1);
          if (!line) continue;
          const ev = JSON.parse(line);
          if (ev.type === "conversation") {
            setConvId(ev.id);
            window.history.replaceState(null, "", `/chat?c=${ev.id}`);
          } else if (ev.type === "text") patchLast((m) => ({ ...m, text: m.text + ev.delta }));
          else if (ev.type === "tool_start") patchLast((m) => ({ ...m, tools: [...(m.tools ?? []), ev.tool] }));
          else if (ev.type === "tool_end")
            patchLast((m) => ({ ...m, tools: (m.tools ?? []).map((t) => (t.id === ev.tool.id ? ev.tool : t)) }));
          else if (ev.type === "action") patchLast((m) => ({ ...m, actions: [...(m.actions ?? []), ev.action] }));
          else if (ev.type === "masked")
            setMessages((ms) => {
              const copy = [...ms];
              const i = copy.length - 2; // the user's message sits just before the streaming reply
              if (copy[i]?.role === "user") {
                const what = Object.entries(ev.counts as Record<string, number>)
                  .map(([k, v]) => `${v} ${k.toLowerCase().replace(/_/g, " ")}`)
                  .join(", ");
                copy[i] = { ...copy[i], text: ev.text, maskedNote: `Masked before sending: ${what}` };
              }
              return copy;
            });
          else if (ev.type === "final_text") patchLast((m) => ({ ...m, text: ev.text }));
          else if (ev.type === "error") setError(ev.message);
        }
      }
    } catch (e) {
      if (!(e instanceof DOMException && e.name === "AbortError")) setError(e instanceof Error ? e.message : String(e));
    } finally {
      patchLast((m) => ({ ...m, streaming: false }));
      setStreaming(false);
      abortRef.current = null;
      void refreshConvs();
      inputRef.current?.focus();
    }
  };

  const updateAction = (msgIndex: number, a: PendingActionView) =>
    setMessages((ms) => ms.map((m, i) => (i === msgIndex ? { ...m, actions: (m.actions ?? []).map((x) => (x.id === a.id ? a : x)) } : m)));

  const deleteConv = async (id: string) => {
    if (!window.confirm("Delete this conversation?")) return;
    await fetch(`/api/assistant/conversations/${id}`, { method: "DELETE", headers: headers() });
    if (id === convId) newChat();
    void refreshConvs();
  };

  if (user === null) return null; // reading localStorage
  if (!user)
    return (
      <NamePrompt
        onSave={(n) => {
          store.set("wo-user", n);
          setUser(n);
        }}
      />
    );

  const scopeName = agents.find((a) => a.id === agentId)?.module;

  return (
    <div className="chat">
      <aside className="chat-rail" aria-label="Conversations">
        <button className="btn primary" onClick={newChat} disabled={streaming} style={{ width: "100%" }}>
          <MessageSquarePlus size={16} aria-hidden /> New chat
        </button>
        <div className="nav-label" style={{ padding: "12px 4px 4px" }}>
          Recent
        </div>
        <nav className="conv-list">
          {convs.length === 0 && <div className="hint" style={{ padding: "0 4px" }}>No conversations yet.</div>}
          {convs.map((c) => (
            <div key={c.id} className="conv-item" aria-current={c.id === convId ? "page" : undefined}>
              <button className="conv-open" onClick={() => openConv(c.id)} title={c.title}>
                {c.title}
              </button>
              <button className="btn ghost small conv-del" aria-label={`Delete conversation ${c.title}`} onClick={() => deleteConv(c.id)}>
                <Trash2 size={13} aria-hidden />
              </button>
            </div>
          ))}
        </nav>
        <div className="hint" style={{ marginTop: "auto", padding: "8px 4px 0" }}>
          Signed as <strong>{user}</strong> ·{" "}
          <button
            className="link-btn"
            onClick={() => {
              store.set("wo-user", "");
              setUser("");
            }}
          >
            change
          </button>
        </div>
      </aside>

      <section className="chat-main" aria-label="Wisdom Assistant">
        <header className="chat-head">
          <div className="row" style={{ gap: 10 }}>
            <span className="brand-mark" aria-hidden>
              <Sparkles size={16} />
            </span>
            <div>
              <h1 style={{ fontSize: 17 }}>Wisdom Assistant</h1>
              <div className="hint" style={{ margin: 0 }}>
                Answers from your changes, test runs, incidents and module wisdom. Actions always ask first.
              </div>
            </div>
          </div>
          <div className="row">
            <label htmlFor="scope" className="hint" style={{ margin: 0 }}>
              Scope
            </label>
            <select
              id="scope"
              value={agentId}
              style={{ width: "auto", minWidth: 180 }}
              onChange={(e) => {
                setAgentId(e.target.value);
                store.set("wo-agent", e.target.value);
              }}
            >
              <option value="">All modules</option>
              {agents.map((a) => (
                <option key={a.id} value={a.id}>
                  {a.module} · {a.name}
                </option>
              ))}
            </select>
          </div>
        </header>

        <div className="chat-scroll" role="log" aria-live="polite" aria-busy={streaming}>
          {messages.length === 0 ? (
            <div className="chat-empty">
              <div style={{ maxWidth: 560, width: "100%" }}>
                <h2 style={{ fontSize: 20, marginBottom: 6 }}>What do you want to know{scopeName ? ` about ${scopeName}` : ""}?</h2>
                <p className="hint" style={{ marginTop: 0 }}>
                  Ask about recent changes, failing tests, incidents or how something works. You can also set up or change an agent.
                </p>
                <div className="suggest-grid">
                  {SUGGESTIONS.map((s) => (
                    <button key={s} className="suggest" onClick={() => send(s)}>
                      {s}
                    </button>
                  ))}
                </div>
              </div>
            </div>
          ) : (
            <div className="msgs">
              {messages.map((m, i) =>
                m.role === "user" ? (
                  <div key={i} className="msg user">
                    <div className="bubble">{m.text}</div>
                    {m.maskedNote && (
                      <div className="masked-note">
                        <ShieldCheck size={13} aria-hidden /> {m.maskedNote}
                      </div>
                    )}
                  </div>
                ) : (
                  <div key={i} className="msg assistant">
                    <span className="avatar" aria-hidden>
                      <Bot size={16} />
                    </span>
                    <div className="msg-body">
                      <ToolTrail tools={m.tools ?? []} streaming={m.streaming} />
                      {m.text ? (
                        <div className="md">
                          <Markdown
                            components={{
                              a: ({ href, children }) => (
                                <a href={href} target={href?.startsWith("http") ? "_blank" : undefined} rel="noreferrer">
                                  {children}
                                </a>
                              ),
                            }}
                          >
                            {linkify(m.text)}
                          </Markdown>
                        </div>
                      ) : (
                        m.streaming &&
                        !(m.tools ?? []).length && (
                          <div className="hint row" style={{ gap: 6 }}>
                            <LoaderCircle size={14} className="spin" aria-hidden /> Thinking…
                          </div>
                        )
                      )}
                      {(m.actions ?? []).map((a) => (
                        <ActionCard key={a.id} action={a} user={user} onUpdate={(na) => updateAction(i, na)} />
                      ))}
                    </div>
                  </div>
                ),
              )}
              <div ref={endRef} />
            </div>
          )}
        </div>

        {error && (
          <div className="error" role="alert" style={{ margin: "0 20px 8px" }}>
            <CircleAlert size={16} aria-hidden />
            <div>{error}</div>
          </div>
        )}

        <form
          className="composer"
          onSubmit={(e) => {
            e.preventDefault();
            void send(input);
          }}
        >
          <label htmlFor="chat-input" className="sr-only">
            Message Wisdom Assistant
          </label>
          <textarea
            id="chat-input"
            ref={inputRef}
            rows={1}
            value={input}
            placeholder={scopeName ? `Ask about ${scopeName}…` : "Ask about any module…"}
            onChange={(e) => setInput(e.target.value)}
            onKeyDown={(e) => {
              if (e.key === "Enter" && !e.shiftKey && !e.nativeEvent.isComposing) {
                e.preventDefault();
                void send(input);
              }
            }}
          />
          {streaming ? (
            <button type="button" className="btn" onClick={() => abortRef.current?.abort()} aria-label="Stop generating">
              <Square size={14} aria-hidden /> Stop
            </button>
          ) : (
            <button className="btn primary" disabled={!input.trim()} aria-label="Send">
              <ArrowUp size={16} aria-hidden />
            </button>
          )}
        </form>
        <div className="composer-hint hint">Enter to send · Shift+Enter for a new line · Client data is masked before it reaches the model.</div>
      </section>
    </div>
  );
}
