// libs/utils/src/index.ts

import chunk from 'lodash-es/chunk';
import uniq from 'lodash-es/uniq';
import flatten from 'lodash-es/flatten';
import intersection from 'lodash-es/intersection';
import difference from 'lodash-es/difference';
import groupBy from 'lodash-es/groupBy';
import filter from 'lodash-es/filter';
import find from 'lodash-es/find';
import map from 'lodash-es/map';
import sortBy from 'lodash-es/sortBy';
import compact from 'lodash-es/compact';
import shuffle from 'lodash-es/shuffle';
import pick from 'lodash-es/pick';
import omit from 'lodash-es/omit';
import merge from 'lodash-es/merge';
import cloneDeep from 'lodash-es/cloneDeep';
import get from 'lodash-es/get';
import has from 'lodash-es/has';
import camelCase from 'lodash-es/camelCase';
import snakeCase from 'lodash-es/snakeCase';
import kebabCase from 'lodash-es/kebabCase';
import startCase from 'lodash-es/startCase';
import truncate from 'lodash-es/truncate';
import capitalize from 'lodash-es/capitalize';
import clamp from 'lodash-es/clamp';
import random from 'lodash-es/random';
import round from 'lodash-es/round';
import debounce from 'lodash-es/debounce';
import throttle from 'lodash-es/throttle';
import memoize from 'lodash-es/memoize';
import negate from 'lodash-es/negate';
import once from 'lodash-es/once';
import isEmpty from 'lodash-es/isEmpty';

export const arrayUtils = {
  chunk,
  uniq,
  flatten,
  intersection,
  difference,
  groupByArray: groupBy,
  filterArray: filter,
  findInArray: find,
  mapArray: map,
  sortArray: sortBy,
  compact,
  shuffle,
  groupBySize: <T>(array: T[], size: number): T[][] => {
    return chunk(array, size);
  }
};

export const objectUtils = {
  pick,
  omit,
  merge,
  cloneDeep,
  get,
  has,
  invert: <T extends Record<string, PropertyKey>>(obj: T): Partial<Record<string, keyof T>> => {
    return Object.entries(obj).reduce((acc, [key, value]) => {
      if (value !== null && value !== undefined) {
        const stringValue = String(value);
        acc[stringValue] = key;
      }
      return acc;
    }, {} as Partial<Record<string, keyof T>>);
  }
};

export const stringUtils = {
  camelCase,
  snakeCase,
  kebabCase,
  startCase,
  truncate,
  capitalize,
  reverse: (str: string): string => str.split('').reverse().join(''),
  isPalindrome: (str: string): boolean => {
    const normalized = str.toLowerCase().replace(/[^a-z0-9]/g, '');
    return normalized === normalized.split('').reverse().join('');
  }
};

export const numberUtils = {
  clamp,
  random,
  round,
  formatCurrency: (num: number, currency: string = 'USD', locale: string = 'en-US'): string => {
    return new Intl.NumberFormat(locale, { style: 'currency', currency }).format(num);
  }
};

export const functionUtils = {
  debounce,
  throttle,
  memoize,
  negate,
  once
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
  }
};

export const commonUtils = {
  isEmpty,
  isNotEmpty: <T>(value: T): boolean => !isEmpty(value),
  isEmptyDetailed: (value: any): boolean => {
    if (isEmpty(value)) return true;
    if (typeof value === 'number') return isNaN(value);
    return false;
  },
  isNotEmptyDetailed: (value: any): boolean => !commonUtils.isEmptyDetailed(value)
};

export const performanceUtils = {
  measureExecutionTime: <T>(fn: () => T): [T, number] => {
    const start = performance.now();
    const result = fn();
    const end = performance.now();
    return [result, end - start];
  }
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
  }
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
  ...jsonUtils
};

export default utils;


-------------

  // libs/utils/src/index.ts

// Efficient Lodash imports
import chunk from 'lodash-es/chunk';
import uniq from 'lodash-es/uniq';
import flatten from 'lodash-es/flatten';
import intersection from 'lodash-es/intersection';
import difference from 'lodash-es/difference';
import groupBy from 'lodash-es/groupBy';
import cloneDeep from 'lodash-es/cloneDeep';
import debounce from 'lodash-es/debounce';
import throttle from 'lodash-es/throttle';
import memoize from 'lodash-es/memoize';

// Array Utilities
export const arrayUtils = {
  chunk,
  uniq,
  flatten,
  intersection,
  difference,
  groupByArray: groupBy,
  filterArray: <T>(array: T[], predicate: (value: T, index: number, array: T[]) => boolean): T[] => 
    array.filter(predicate),
  findInArray: <T>(array: T[], predicate: (value: T, index: number, array: T[]) => boolean): T | undefined => 
    array.find(predicate),
  mapArray: <T, U>(array: T[], callbackfn: (value: T, index: number, array: T[]) => U): U[] => 
    array.map(callbackfn),
  sortArray: <T>(array: T[], compareFunction?: (a: T, b: T) => number): T[] => 
    [...array].sort(compareFunction),
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

// Object Utilities
export const objectUtils = {
  cloneDeep,
  pick: <T extends object, K extends keyof T>(obj: T, keys: K[]): Pick<T, K> => 
    keys.reduce((acc, key) => (key in obj && (acc[key] = obj[key]), acc), {} as Pick<T, K>),
  omit: <T extends object, K extends keyof T>(obj: T, keys: K[]): Omit<T, K> => {
    const result = { ...obj };
    keys.forEach(key => delete result[key]);
    return result;
  },
  merge: <T extends object>(...objects: T[]): T => Object.assign({}, ...objects),
  has: (obj: object, path: string): boolean => {
    const parts = path.split('.');
    let current: any = obj;
    for (const part of parts) {
      if (current == null || !Object.prototype.hasOwnProperty.call(current, part)) {
        return false;
      }
      current = current[part];
    }
    return true;
  },
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

// String Utilities
export const stringUtils = {
  camelCase: (str: string): string => 
    str.replace(/(?:^\w|[A-Z]|\b\w)/g, (word, index) => 
      index === 0 ? word.toLowerCase() : word.toUpperCase()
    ).replace(/\s+/g, ''),
  snakeCase: (str: string): string => 
    str.replace(/\W+/g, ' ')
      .split(/ |\B(?=[A-Z])/)
      .map(word => word.toLowerCase())
      .join('_'),
  kebabCase: (str: string): string => 
    str.replace(/\W+/g, ' ')
      .split(/ |\B(?=[A-Z])/)
      .map(word => word.toLowerCase())
      .join('-'),
  startCase: (str: string): string => 
    str.replace(/\W+/g, ' ')
      .split(/ |\B(?=[A-Z])/)
      .map(word => word.charAt(0).toUpperCase() + word.slice(1).toLowerCase())
      .join(' '),
  truncate: (str: string, length: number, ending: string = '...'): string => 
    str.length > length ? str.slice(0, length - ending.length) + ending : str,
  capitalize: (str: string): string => str.charAt(0).toUpperCase() + str.slice(1),
  reverse: (str: string): string => str.split('').reverse().join(''),
  isPalindrome: (str: string): boolean => {
    const normalized = str.toLowerCase().replace(/[^a-z0-9]/g, '');
    return normalized === normalized.split('').reverse().join('');
  },
};

// Number Utilities
export const numberUtils = {
  clamp: (number: number, lower: number, upper: number): number => 
    Math.min(Math.max(number, lower), upper),
  random: (min: number, max: number): number => 
    Math.floor(Math.random() * (max - min + 1) + min),
  round: (num: number, decimals: number): number => {
    return Number(Math.round(Number(num + 'e' + decimals)) + 'e-' + decimals);
  },
  formatCurrency: (num: number, currency: string = 'USD', locale: string = 'en-US'): string => {
    return new Intl.NumberFormat(locale, { style: 'currency', currency }).format(num);
  },
};

// Function Utilities
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

// Crypto Utilities
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

// Common Utilities
export const commonUtils = {
  isEmpty: (value: any): boolean => {
    if (value == null) {
      return true;
    }
    if (typeof value === 'string' || Array.isArray(value)) {
      return value.length === 0;
    }
    if (typeof value === 'object') {
      return Object.keys(value).length === 0;
    }
    return false;
  },
  isNotEmpty: <T>(value: T): boolean => !commonUtils.isEmpty(value),
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
  isNotEmptyDetailed: (value: any): boolean => !commonUtils.isNotEmptyDetailed(value),
};

// Performance Utilities
export const performanceUtils = {
  measureExecutionTime: <T>(fn: () => T): [T, number] => {
    const start = performance.now();
    const result = fn();
    const end = performance.now();
    return [result, end - start];
  },
};

// JSON Utilities
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

// Main utils export
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
};

export default utils;
