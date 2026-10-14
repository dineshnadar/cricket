import { NextResponse } from "next/server";
import type { ObjectId } from "mongodb";
import { toId } from "./db";
import { errMsg } from "./util";

export function ok<T>(data: T, status = 200) {
  return NextResponse.json(data, { status });
}

export function fail(message: string, status = 400) {
  return NextResponse.json({ error: message }, { status });
}

export async function idParam(params: Promise<{ id: string }>): Promise<ObjectId | null> {
  const { id } = await params;
  return toId(id);
}

/** Wrap a handler so thrown errors become JSON 500s. */
export function handle<A extends unknown[]>(fn: (...args: A) => Promise<Response>) {
  return async (...args: A): Promise<Response> => {
    try {
      return await fn(...args);
    } catch (e) {
      console.error(e);
      return fail(errMsg(e), 500);
    }
  };
}
