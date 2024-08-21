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
