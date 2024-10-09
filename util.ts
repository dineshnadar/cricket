// utils.ts

// Individual imports from lodash-es for better tree-shaking
import camelCase from 'lodash-es/camelCase';
import snakeCase from 'lodash-es/snakeCase';
import kebabCase from 'lodash-es/kebabCase';
import startCase from 'lodash-es/startCase';
import truncate from 'lodash-es/truncate';
import capitalize from 'lodash-es/capitalize';
import trim from 'lodash-es/trim';
import upperCase from 'lodash-es/upperCase';
import lowerCase from 'lodash-es/lowerCase';
import upperFirst from 'lodash-es/upperFirst';
import toLower from 'lodash-es/toLower';
import toUpper from 'lodash-es/toUpper';
import padStart from 'lodash-es/padStart';
import padEnd from 'lodash-es/padEnd';
import repeat from 'lodash-es/repeat';
import replace from 'lodash-es/replace';
import chunk from 'lodash-es/chunk';
import uniq from 'lodash-es/uniq';
import uniqBy from 'lodash-es/uniqBy';
import flatten from 'lodash-es/flatten';
import intersection from 'lodash-es/intersection';
import difference from 'lodash-es/difference';
import isEmpty from 'lodash-es/isEmpty';
import filter from 'lodash-es/filter';
import find from 'lodash-es/find';
import map from 'lodash-es/map';
import reduce from 'lodash-es/reduce';
import sortBy from 'lodash-es/sortBy';
import groupBy from 'lodash-es/groupBy';
import pick from 'lodash-es/pick';
import omit from 'lodash-es/omit';
import merge from 'lodash-es/merge';
import cloneDeep from 'lodash-es/cloneDeep';
import get from 'lodash-es/get';
import set from 'lodash-es/set';
import has from 'lodash-es/has';
import clamp from 'lodash-es/clamp';
import random from 'lodash-es/random';
import round from 'lodash-es/round';
import sum from 'lodash-es/sum';
import mean from 'lodash-es/mean';
import debounce from 'lodash-es/debounce';
import throttle from 'lodash-es/throttle';
import memoize from 'lodash-es/memoize';

// String Utilities
export const stringUtils = {
  camelCase,
  snakeCase,
  kebabCase,
  startCase,
  capitalize,
  trim,
  upperCase,
  lowerCase,
  upperFirst,
  toLower,
  toUpper,
  padStart,
  padEnd,
  repeat,
  replace,
  truncate,
  reverse: (str: string): string => str.split('').reverse().join(''),
  isPalindrome: (str: string): boolean => {
    const normalized = str.toLowerCase().replace(/[^a-z0-9]/g, '');
    return normalized === normalized.split('').reverse().join('');
  },
  toTitleCase: (str: string): string => str.replace(/\w\S*/g, txt => txt.charAt(0).toUpperCase() + txt.slice(1).toLowerCase()),
  removeNonAlphanumeric: (str: string): string => str.replace(/[^a-zA-Z0-9]/g, ''),
  countOccurrences: (str: string, subString: string): number => (str.match(new RegExp(subString, 'g')) || []).length,
  slugify: (str: string): string => str.toLowerCase().trim().replace(/[^\w\s-]/g, '').replace(/[\s_-]+/g, '-').replace(/^-+|-+$/g, ''),
  stripHtml: (str: string): string => str.replace(/<[^>]*>/g, ''),
  isEmail: (str: string): boolean => /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(str),
  isUrl: (str: string): boolean => /^(https?:\/\/)?([\da-z\.-]+)\.([a-z\.]{2,6})([\/\w \.-]*)*\/?$/.test(str),
  extractEmails: (str: string): string[] => str.match(/([a-zA-Z0-9._-]+@[a-zA-Z0-9._-]+\.[a-zA-Z0-9_-]+)/gi) || [],
  mask: (str: string, maskChar: string = '*', unmaskedStart: number = 0, unmaskedEnd: number = 0): string => {
    return str.slice(0, unmaskedStart) + maskChar.repeat(str.length - unmaskedStart - unmaskedEnd) + str.slice(str.length - unmaskedEnd);
  },
  wordCount: (str: string): number => str.trim().split(/\s+/).length
};

// Array Utilities
export const arrayUtils = {
  chunk,
  uniq,
  uniqBy,
  flatten,
  intersection,
  difference,
  isEmpty,
  filterArray: filter,
  findInArray: find,
  mapArray: map,
  reduceArray: reduce,
  sortArray: sortBy,
  groupByArray: groupBy,
  compact: <T>(array: T[]): T[] => array.filter(Boolean),
  shuffle: <T>(array: T[]): T[] => {
    const shuffled = [...array];
    for (let i = shuffled.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [shuffled[i], shuffled[j]] = [shuffled[j], shuffled[i]];
    }
    return shuffled;
  },
  range: (start: number, end: number, step: number = 1): number[] => {
    const length = Math.max(Math.ceil((end - start) / step), 0);
    return Array(length).fill(0).map((_, index) => start + index * step);
  },
  zip: <T>(...arrays: T[][]): T[][] => {
    const length = Math.min(...arrays.map(arr => arr.length));
    return Array(length).fill(0).map((_, i) => arrays.map(arr => arr[i]));
  }
};

// Object Utilities
export const objectUtils = {
  pick,
  omit,
  merge,
  cloneDeep,
  get,
  set,
  has,
  invert: <T extends Record<string, PropertyKey>>(obj: T): Partial<Record<string, keyof T>> => {
    return Object.entries(obj).reduce((acc, [key, value]) => {
      if (value != null) {
        acc[String(value)] = key;
      }
      return acc;
    }, {} as Partial<Record<string, keyof T>>);
  },
  deepMerge: <T>(target: T, ...sources: Partial<T>[]): T => {
    if (!sources.length) return target;
    const source = sources.shift();
    if (source === undefined) return target;

    if (isMergeableObject(target) && isMergeableObject(source)) {
      Object.keys(source).forEach(key => {
        if (isMergeableObject(source[key])) {
          if (!target[key]) Object.assign(target, { [key]: {} });
          deepMerge(target[key], source[key]);
        } else {
          Object.assign(target, { [key]: source[key] });
        }
      });
    }

    return deepMerge(target, ...sources);
  }
};

// Number Utilities
export const numberUtils = {
  clamp,
  random,
  round,
  sum,
  mean,
  formatCurrency: (value: number | string, currency: string = 'USD', locale: string = 'en-US'): string => {
    const num = typeof value === 'string' ? parseFloat(value) : value;
    if (isNaN(num)) {
      throw new Error('Invalid number for currency formatting');
    }
    return new Intl.NumberFormat(locale, { 
      style: 'currency', 
      currency,
      minimumFractionDigits: 0,
      maximumFractionDigits: 2
    }).format(num);
  },
  isNumeric: (value: any): boolean => !isNaN(parseFloat(value)) && isFinite(value),
  toFixed: (num: number, decimals: number): string => num.toFixed(decimals),
  toPrecision: (num: number, precision: number): string => num.toPrecision(precision),
  toPercentage: (num: number, decimals: number = 2): string => `${(num * 100).toFixed(decimals)}%`
};

// Function Utilities
export const functionUtils = {
  debounce,
  throttle,
  memoize,
  negate: <T>(predicate: (value: T) => boolean) => (value: T): boolean => !predicate(value),
  once: <T extends (...args: any[]) => any>(fn: T): T => {
    let called = false;
    let result: ReturnType<T>;
    return ((...args: Parameters<T>): ReturnType<T> => {
      if (!called) {
        called = true;
        result = fn(...args);
      }
      return result;
    }) as T;
  },
  compose: (...fns: Function[]) => (x: any) => fns.reduceRight((v, f) => f(v), x),
  pipe: (...fns: Function[]) => (x: any) => fns.reduce((v, f) => f(v), x)
};

// Helper function for deepMerge
function isMergeableObject(item: any): item is Object {
  return item && typeof item === 'object' && !Array.isArray(item);
}

// Utility application logic
type UtilCategory = 'str' | 'arr' | 'obj' | 'num' | 'func';

type UtilConfig = {
  category: UtilCategory;
  method: string;
  params?: any[];
};

const utilCategories: Record<UtilCategory, Record<string, Function>> = {
  str: stringUtils,
  arr: arrayUtils,
  obj: objectUtils,
  num: numberUtils,
  func: functionUtils
};

const parseParams = (paramString: string): any[] => {
  if (!paramString) return [];
  
  const parts = paramString.match(/(?:[^,{}]+|\{[^}]+\})+/g) || [];
  
  return parts.map(part => {
    part = part.trim();
    if (part.startsWith('{') && part.endsWith('}')) {
      return JSON.parse(part.replace(/(\w+):/g, '"$1":'));
    } else if (part === 'true') {
      return true;
    } else if (part === 'false') {
      return false;
    } else if (part === 'null') {
      return null;
    } else if (part === 'undefined') {
      return undefined;
    } else if (!isNaN(Number(part))) {
      return Number(part);
    } else {
      return part.replace(/^['"]|['"]$/g, '');
    }
  });
};

const parseUtilString = (utilString: string): UtilConfig => {
  const [category, method, paramString] = utilString.split(':');
  const params = parseParams(paramString);

  return { category: category as UtilCategory, method, params };
};

export const applyUtil = (input: any, config: UtilConfig): any => {
  const { category, method, params = [] } = config;
  const utilCategory = utilCategories[category];
  const utilMethod = utilCategory[method];

  if (typeof utilMethod === 'function') {
    return utilMethod(input, ...params);
  }

  return input;
};

export const applyUtilFromString = (input: any, utilString: string): any => {
  const config = parseUtilString(utilString);
  return applyUtil(input, config);
};

export const applyMultipleUtilsFromString = (input: any, utilString: string): any => {
  return utilString.split('|')
    .map(s => s.trim())
    .reduce((result, utilString) => applyUtilFromString(result, utilString), input);
};

export const smartApplyUtils = (input: any, utilString: string): any => {
  return utilString.includes('|') ? applyMultipleUtilsFromString(input, utilString) : applyUtilFromString(input, utilString);
};

// Export all utilities
export const utils = {
  ...stringUtils,
  ...arrayUtils,
  ...objectUtils,
  ...numberUtils,
  ...functionUtils,
  applyUtil,
  applyUtilFromString,
  applyMultipleUtilsFromString,
  smartApplyUtils
};

export default utils;
