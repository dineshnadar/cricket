
export type HttpMethod = 'GET' | 'POST' | 'PUT' | 'PATCH' | 'DELETE';

export interface ApiEndpointConfig {
  url: string | ((params: Record<string, any>) => string);
  method: HttpMethod;
  pathParams?: string[];
  queryParams?: string[];
  headers?: Record<string, string>;
  withCredentials?: boolean;
  shouldCache?: boolean;
  cacheType?: CacheType;
  cacheTimeout?: number;
  requestTimeout?: number;
  retryEnabled?: boolean;
  maxRetries?: number;
}

export const API_CONFIG = {
  baseUrl: 'https://api.example.com',
  defaultHeaders: {
    'Content-Type': 'application/json'
  },
  endpoints: {
    getUsers: {
      url: '/users',
      method: 'GET',
      shouldCache: true,
      cacheType: 'memory' as CacheType,
      cacheTimeout: 300000, // 5 minutes
      queryParams: ['page', 'limit']
    },
    getUser: {
      url: (params) => `/users/${params.userId}`,
      method: 'GET',
      pathParams: ['userId'],
      shouldCache: true,
      cacheType: 'local' as CacheType,
      cacheTimeout: 600000 // 10 minutes
    },
    addUser: {
      url: '/users',
      method: 'POST',
      shouldCache: false
    },
    updateUser: {
      url: (params) => `/users/${params.userId}`,
      method: 'PUT',
      pathParams: ['userId'],
      shouldCache: false
    },
    deleteUser: {
      url: (params) => `/users/${params.userId}`,
      method: 'DELETE',
      pathParams: ['userId'],
      shouldCache: false
    },
    uploadFile: {
      url: '/files/upload',
      method: 'POST',
      headers: { 'Content-Type': 'multipart/form-data' },
      shouldCache: false
    }
  }
};

export type EndpointKey = keyof typeof API_CONFIG.endpoints;

export function validateParams(
  config: ApiEndpointConfig,
  pathParams?: Record<string, any>,
  queryParams?: Record<string, any>
): void {
  if (config.pathParams) {
    for (const param of config.pathParams) {
      if (!pathParams || pathParams[param] === undefined) {
        throw new Error(`Missing required path parameter: ${param}`);
      }
    }
  }

  if (config.queryParams && queryParams) {
    for (const param in queryParams) {
      if (!config.queryParams.includes(param)) {
        console.warn(`Unexpected query parameter: ${param}`);
      }
    }
  }
}

export function buildUrl(config: ApiEndpointConfig, pathParams?: Record<string, any>): string {
  let url = typeof config.url === 'function' ? config.url(pathParams || {}) : config.url;
  return `${API_CONFIG.baseUrl}${url}`;
}
---------
  import { Injectable, signal } from '@angular/core';

export type CacheType = 'memory' | 'local' | 'session';

interface CacheEntry<T> {
  data: T;
  expiry?: number;
  timestamp: number; // Add timestamp
}

@Injectable({
  providedIn: 'root'
})
export class CacheService {
  private memoryCache = signal<Map<string, CacheEntry<any>>>(new Map());

  set<T>(key: string, data: T, expirationInMs?: number, type: CacheType = 'memory'): void {
    const entry: CacheEntry<T> = { data, timestamp: Date.now() };
    if (expirationInMs !== undefined) {
      entry.expiry = Date.now() + expirationInMs;
    }

    switch (type) {
      case 'local':
        localStorage.setItem(key, JSON.stringify(entry));
        break;
      case 'session':
        sessionStorage.setItem(key, JSON.stringify(entry));
        break;
      default:
        this.memoryCache.update(cache => {
          cache.set(key, entry);
          return new Map(cache);
        });
    }
  }

  get<T>(key: string, type: CacheType = 'memory'): T | null {
    let entry: CacheEntry<T> | null = null;

    switch (type) {
      case 'local':
        const localData = localStorage.getItem(key);
        entry = localData ? JSON.parse(localData) : null;
        break;
      case 'session':
        const sessionData = sessionStorage.getItem(key);
        entry = sessionData ? JSON.parse(sessionData) : null;
        break;
      default:
        entry = this.memoryCache().get(key) || null;
    }

    if (!entry) {
      return null;
    }

    if (entry.expiry && Date.now() > entry.expiry) {
      this.remove(key, type);
      return null;
    }

    return entry.data;
  }

  remove(key: string, type: CacheType = 'memory'): void {
    switch (type) {
      case 'local':
        localStorage.removeItem(key);
        break;
      case 'session':
        sessionStorage.removeItem(key);
        break;
      default:
        this.memoryCache.update(cache => {
          cache.delete(key);
          return new Map(cache);
        });
    }
  }

  clear(type: CacheType = 'memory'): void {
    switch (type) {
      case 'local':
        localStorage.clear();
        break;
      case 'session':
        sessionStorage.clear();
        break;
      default:
        this.memoryCache.set(new Map());
    }
  }

  // New method to check if a request with the same parameters is cached
  hasRequestWithSameParams(key: string, type: CacheType = 'memory'): boolean {
    let entry: CacheEntry<any> | null = null;

    switch (type) {
      case 'local':
        const localData = localStorage.getItem(key);
        entry = localData ? JSON.parse(localData) : null;
        break;
      case 'session':
        const sessionData = sessionStorage.getItem(key);
        entry = sessionData ? JSON.parse(sessionData) : null;
        break;
      default:
        entry = this.memoryCache().get(key) || null;
    }

    if (!entry) {
      return false;
    }

    if (entry.expiry && Date.now() > entry.expiry) {
      this.remove(key, type);
      return false;
    }

    return true;
  }
}
-------------
  import { Injectable } from '@angular/core';
import { signal } from '@angular/core';

interface ActivityLog {
  type: 'api' | 'click' | 'other';
  details: any;
  timestamp: number;
}

@Injectable({
  providedIn: 'root'
})
export class ActivityTrackingService {
  private activityLogs = signal<ActivityLog[]>([]);

  logActivity(type: 'api' | 'click' | 'other', details: any): void {
    const log: ActivityLog = {
      type,
      details,
      timestamp: Date.now()
    };
    this.activityLogs.update(logs => [...logs, log]);
  }

  getActivityLogs(): ActivityLog[] {
    return this.activityLogs();
  }
}
------------------

  import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class LoggingService {
  log(message: string, data?: any): void {
    console.log(message, data);
  }

  error(message: string, data?: any): void {
    console.error(message, data);
  }

  warn(message: string, data?: any): void {
    console.warn(message, data);
  }

  info(message: string, data?: any): void {
    console.info(message, data);
  }
}
-----------
  import { Injectable, inject, signal, computed } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Observable, throwError, of } from 'rxjs';
import { catchError, tap, retry, timeout, finalize, takeUntil } from 'rxjs/operators';
import { API_CONFIG, ApiEndpointConfig, EndpointKey, validateParams, buildUrl, HttpMethod } from './api.config';
import { CacheService, CacheType } from './cache.service';
import { HttpService } from './http.service';
import { ActivityTrackingService } from './activity-tracking.service';
import { LoggingService } from './logging.service';

@Injectable({
  providedIn: 'root'
})
export class ApiService {
  private httpService = inject(HttpService);
  private cacheService = inject(CacheService);
  private activityTrackingService = inject(ActivityTrackingService);
  private loggingService = inject(LoggingService);

  private pendingRequests = signal<Map<string, AbortController>>(new Map());
  isLoading = computed(() => this.pendingRequests().size > 0);

  request<T>(endpointKey: EndpointKey, data?: any, pathParams?: Record<string, any>, queryParams?: Record<string, any>): Observable<T> {
    const config = API_CONFIG.endpoints[endpointKey];
    if (!config) {
      return throwError(() => new Error(`No configuration found for endpoint: ${endpointKey}`));
    }

    try {
      validateParams(config, pathParams, queryParams);
    } catch (error) {
      return throwError(() => error);
    }

    const url = buildUrl(config, pathParams);
    const cacheKey = this.generateCacheKey(endpointKey, config.method, pathParams, queryParams, data);

    if (config.shouldCache) {
      // Check if the request is already in the cache
      if (this.cacheService.hasRequestWithSameParams(cacheKey, config.cacheType)) {
        const cachedData = this.cacheService.get<T>(cacheKey, config.cacheType);
        if (cachedData !== null) {
          this.logApiCall(config, url, data, pathParams, queryParams, cachedData, true);
          return of(cachedData);
        }
      }
    }

    // Check if an identical request is already in progress
    if (this.pendingRequests().has(cacheKey)) {
      return throwError(() => new Error(`Request with identical parameters is already in progress: ${cacheKey}`));
    }

    const abortController = new AbortController();
    const apiCall$ = this.makeApiCall<T>(config, url, cacheKey, data, pathParams, queryParams, abortController);
    this.pendingRequests.update(map => {
      map.set(cacheKey, abortController);
      return new Map(map);
    });

    return apiCall$;
  }

  private makeApiCall<T>(
    config: ApiEndpointConfig, 
    url: string, 
    cacheKey: string, 
    data?: any, 
    pathParams?: Record<string, any>, 
    queryParams?: Record<string, any>, 
    abortController: AbortController
  ): Observable<T> {
    const options = this.getRequestOptions(config, queryParams, data);
    options.signal = abortController.signal;

    return this.httpService.request<T>(config.method, url, options).pipe(
      timeout(config.requestTimeout || 30000),
      retry(config.retryEnabled ? config.maxRetries || 3 : 0),
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

  private createCancelationObservable(cacheKey: string): Observable<void> {
    return new Observable<void>(observer => {
      const intervalId = setInterval(() => {
        if (!this.pendingRequests().has(cacheKey)) {
          clearInterval(intervalId);
          observer.next();
          observer.complete();
        }
      }, 100);

      return () => clearInterval(intervalId);
    });
  }

  private generateCacheKey(
    endpointKey: EndpointKey,
    method: HttpMethod,
    pathParams?: Record<string, any>,
    queryParams?: Record<string, any>,
    data?: any
  ): string {
    const parts = [method, endpointKey];
    if (pathParams) {
      parts.push(JSON.stringify(pathParams));
    }
    if (queryParams) {
      parts.push(JSON.stringify(queryParams));
    }
    if (data && (method === 'POST' || method === 'PUT' || method === 'PATCH')) {
      parts.push(JSON.stringify(data));
    }
    return parts.join(':');
  }

  private getRequestOptions(config: ApiEndpointConfig, queryParams?: Record<string, any>, data?: any): any {
    const options: any = {
      headers: { ...API_CONFIG.defaultHeaders, ...config.headers },
      withCredentials: config.withCredentials
    };

    if (queryParams) {
      options.params = queryParams;
    }

    if (data && (config.method === 'POST' || config.method === 'PUT' || config.method === 'PATCH')) {
      options.body = data;
    }

    return options;
  }

  private logApiCall(
    config: ApiEndpointConfig, 
    url: string, 
    data: any, 
    pathParams: Record<string, any>, 
    queryParams: Record<string, any>, 
    response: any, 
    fromCache: boolean
  ): void {
    this.activityTrackingService.logActivity('api', {
      endpointKey: config.url.toString(),
      method: config.method,
      url,
      data,
      pathParams,
      queryParams,
      response,
      fromCache
    });
  }

  private handleError(error: HttpErrorResponse) {
    this.loggingService.error('API error', error);
    return throwError(() => error);
  }

  clearCache(endpointKey: EndpointKey, pathParams?: Record<string, any>, queryParams?: Record<string, any>, data?: any): void {
    const config = API_CONFIG.endpoints[endpointKey];
    const cacheKey = this.generateCacheKey(endpointKey, config.method, pathParams, queryParams, data);
    this.cacheService.remove(cacheKey, config.cacheType);
  }

  cancelRequest(endpointKey: EndpointKey, pathParams?: Record<string, any>, queryParams?: Record<string, any>, data?: any): void {
    const config = API_CONFIG.endpoints[endpointKey];
    const cacheKey = this.generateCacheKey(endpointKey, config.method, pathParams, queryParams, data);
    const abortController = this.pendingRequests().get(cacheKey);
    if (abortController) {
      abortController.abort();
      this.pendingRequests.update(map => {
        map.delete(cacheKey);
        return new Map(map);
      });
    }
  }

  cancelAllRequests(): void {
    this.pendingRequests().forEach((abortController, cacheKey) => {
      abortController.abort();
    });
    this.pendingRequests.set(new Map());
  }
}

---------------
  import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Observable, throwError } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { HttpMethod } from './api.config';

@Injectable({
  providedIn: 'root'
})
export class HttpService {
  constructor(private http: HttpClient) {}

  request<T>(method: HttpMethod, url: string, options: any = {}): Observable<T> {
    const { body, ...httpOptions } = options;

    switch (method) {
      case 'GET':
        return this.http.get<T>(url, httpOptions).pipe(catchError(this.handleError));
      case 'POST':
        return this.http.post<T>(url, body, httpOptions).pipe(catchError(this.handleError));
      case 'PUT':
        return this.http.put<T>(url, body, httpOptions).pipe(catchError(this.handleError));
      case 'PATCH':
        return this.http.patch<T>(url, body, httpOptions).pipe(catchError(this.handleError));
      case 'DELETE':
        return this.http.delete<T>(url, httpOptions).pipe(catchError(this.handleError));
      default:
        return throwError(() => new Error(`Unsupported HTTP method: ${method}`));
    }
  }

  upload<T>(url: string, formData: FormData, options: any = {}): Observable<T> {
    const httpOptions = {
      headers: new HttpHeaders({ 'Content-Type': 'multipart/form-data' }),
      ...options
    };

    return this.http.post<T>(url, formData, httpOptions).pipe(catchError(this.handleError));
  }

  private handleError(error: any) {
    console.error('An error occurred:', error);
    return throwError(() => error);
  }
}
