// libs/utils/src/index.ts

import {
  chunk,
  uniq,
  flatten,
  intersection,
  difference,
  isEmpty,
  filter,
  find,
  map,
  sortBy,
  groupBy,
  pick,
  omit,
  merge,
  cloneDeep,
  has,
  camelCase,
  snakeCase,
  kebabCase,
  startCase,
  truncate,
  clamp,
  random,
  debounce,
  throttle,
  memoize,
  sumBy,
  meanBy,
  flowRight
} from 'lodash-es';

export const arrayUtils = {
  chunk,
  uniq,
  flatten,
  intersection,
  difference,
  filterArray: filter,
  findInArray: find,
  mapArray: map,
  sortArray: sortBy,
  groupByArray: groupBy,
  uniqArray: uniq,
  compact: <T>(array: T[]): T[] => array.filter(Boolean),
  groupBySize: <T>(array: T[], size: number): T[][] => {
    return array.reduce((acc, _, i) => {
      if (i % size === 0) acc.push(array.slice(i, i + size));
      return acc;
    }, [] as T[][]);
  },
  shuffle: <T>(array: T[]): T[] => {
    const shuffled = [...array];
    for (let i = shuffled.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [shuffled[i], shuffled[j]] = [shuffled[j], shuffled[i]];
    }
    return shuffled;
  },
};

export const objectUtils = {
  pick,
  omit,
  merge,
  cloneDeep,
  has,
  get: <T>(obj: any, path: string, defaultValue: T): T => {
    const travel = (regexp: RegExp) =>
      String.prototype.split
        .call(path, regexp)
        .filter(Boolean)
        .reduce((res, key) => (res !== null && res !== undefined ? res[key] : res), obj);
    const result = travel(/[,[\]]+?/) || travel(/[,[\].]+?/);
    return result === undefined || result === obj ? defaultValue : result;
  },
  invert: <T extends Record<string, PropertyKey>>(obj: T): Partial<Record<string, keyof T>> => {
    return Object.entries(obj).reduce((acc, [key, value]) => {
      if (value !== null && value !== undefined) {
        const stringValue = String(value);
        acc[stringValue] = key;
      }
      return acc;
    }, {} as Partial<Record<string, keyof T>>);
  },
};

export const stringUtils = {
  camelCase,
  snakeCase,
  kebabCase,
  startCase,
  truncate,
  capitalize: (str: string): string => str.charAt(0).toUpperCase() + str.slice(1),
  reverse: (str: string): string => str.split('').reverse().join(''),
  isPalindrome: (str: string): boolean => {
    const normalized = str.toLowerCase().replace(/[^a-z0-9]/g, '');
    return normalized === normalized.split('').reverse().join('');
  },
};

export const numberUtils = {
  clamp,
  random,
  round: (num: number, decimals: number): number => {
    return Number(Math.round(Number(num + 'e' + decimals)) + 'e-' + decimals);
  },
  formatCurrency: (num: number, currency: string = 'USD', locale: string = 'en-US'): string => {
    return new Intl.NumberFormat(locale, { style: 'currency', currency }).format(num);
  },
};

export const functionUtils = {
  debounce,
  throttle,
  memoize,
  negate: <T>(predicate: (value: T) => boolean) => (value: T): boolean => !predicate(value),
  once: <T extends (...args: any[]) => any>(fn: T): T => {
    let result: ReturnType<T>;
    let called = false;
    return ((...args: Parameters<T>): ReturnType<T> => {
      if (!called) {
        called = true;
        result = fn(...args);
      }
      return result;
    }) as T;
  },
};

export const cryptoUtils = {
  generateGuid: (timestamp: number = Date.now()): string => {
    const hexTimestamp = timestamp.toString(16);
    const randomPart = () => Math.floor(Math.random() * 16).toString(16);
    
    const guidTemplate = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx';
    let index = 0;
    
    return guidTemplate.replace(/[xy]/g, (c) => {
      const r = (index < hexTimestamp.length) 
        ? parseInt(hexTimestamp[index++], 16) 
        : Math.random() * 16 | 0;
      const v = c === 'x' ? r : (r & 0x3 | 0x8);
      return v.toString(16);
    });
  },
};

export const commonUtils = {
  isEmpty,
  isNotEmpty: <T>(value: T): boolean => !isEmpty(value),
  
  isEmptyDetailed: (value: any): boolean => {
    if (value == null) {
      return true;
    }
    if (typeof value === 'string' || Array.isArray(value)) {
      return value.length === 0;
    }
    if (typeof value === 'object') {
      return Object.keys(value).length === 0;
    }
    if (typeof value === 'number') {
      return isNaN(value);
    }
    return false;
  },
  
  isNotEmptyDetailed: (value: any): boolean => !commonUtils.isEmptyDetailed(value),
};

export const performanceUtils = {
  measureExecutionTime: <T>(fn: () => T): [T, number] => {
    const start = performance.now();
    const result = fn();
    const end = performance.now();
    return [result, end - start];
  },
};

export const jsonUtils = {
  compareJSON: (obj1: any, obj2: any): boolean => {
    return JSON.stringify(obj1) === JSON.stringify(obj2);
  },
  findJSONDifference: (obj1: any, obj2: any): object => {
    const result: {[key: string]: any} = {};

    const compareValues = (value1: any, value2: any, key: string) => {
      if (Array.isArray(value1) && Array.isArray(value2)) {
        if (value1.length !== value2.length) {
          result[key] = { obj1: value1, obj2: value2 };
        } else {
          const arrayDiff = value1.reduce((acc: any, _, index) => {
            const diff = jsonUtils.findJSONDifference(value1[index], value2[index]);
            if (Object.keys(diff).length > 0) {
              acc[index] = diff;
            }
            return acc;
          }, {});
          if (Object.keys(arrayDiff).length > 0) {
            result[key] = arrayDiff;
          }
        }
      } else if (typeof value1 === 'object' && value1 !== null && typeof value2 === 'object' && value2 !== null) {
        const nestedDiff = jsonUtils.findJSONDifference(value1, value2);
        if (Object.keys(nestedDiff).length > 0) {
          result[key] = nestedDiff;
        }
      } else if (value1 !== value2) {
        result[key] = { obj1: value1, obj2: value2 };
      }
    };

    for (const key in obj1) {
      if (obj1.hasOwnProperty(key)) {
        if (obj2.hasOwnProperty(key)) {
          compareValues(obj1[key], obj2[key], key);
        } else {
          result[key] = { obj1: obj1[key], obj2: undefined };
        }
      }
    }

    for (const key in obj2) {
      if (obj2.hasOwnProperty(key) && !obj1.hasOwnProperty(key)) {
        result[key] = { obj1: undefined, obj2: obj2[key] };
      }
    }

    return result;
  },
  hasDifference: (obj1: any, obj2: any): boolean => {
    return Object.keys(jsonUtils.findJSONDifference(obj1, obj2)).length > 0;
  },
};

export const utils = {
  ...arrayUtils,
  ...objectUtils,
  ...stringUtils,
  ...numberUtils,
  ...functionUtils,
  ...cryptoUtils,
  ...commonUtils,
  ...performanceUtils,
  ...jsonUtils,
  sumBy,
  meanBy,
  flowRight,
};

export default utils;
