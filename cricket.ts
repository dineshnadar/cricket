private makeApiCall<T>(
  config: ApiEndpointConfig,
  url: string,
  cacheKey: string,
  pathParams: Record<string, any>,
  queryParams: Record<string, any>,
  abortController: AbortController,
  data?: any
): Observable<T> {
  const options = this.getRequestOptions(config, queryParams, data);
  options.signal = abortController.signal;

  return this.httpService.request<T>(config.method, url, options).pipe(
    timeout(config.requestTimeout || 30000),
    retry(config.retryEnabled ? config.maxRetries || 3 : 0),
    // Filter for response events and map to response body
    filter((event: HttpEvent<T>) => event.type === HttpEventType.Response),
    map((event: HttpEvent<T>) => {
      if (event instanceof HttpResponse) {
        return event.body as T;
      }
      throw new Error('Unexpected HTTP event type');
    }),
    catchError(this.handleError),
    tap(response => {
      if (config.shouldCache) {
        this.cacheService.set(cacheKey, response, config.cacheTimeout, config.cacheType);
      }
      this.logApiCall(config, url, data, pathParams, queryParams, response, false);
    }),
    finalize(() => {
      this.pendingRequests.update(map => {
        map.delete(cacheKey);
        return new Map(map);
      });
    }),
    takeUntil(this.createCancelationObservable(cacheKey))
  );
}
