// libs/utils/src/index.ts

// Lodash imports
import chunk from 'lodash/chunk';
import uniq from 'lodash/uniq';
import flatten from 'lodash/flatten';
import intersection from 'lodash/intersection';
import difference from 'lodash/difference';
import isEmpty from 'lodash/isEmpty';
import filter from 'lodash/filter';
import find from 'lodash/find';
import map from 'lodash/map';
import sortBy from 'lodash/sortBy';
import groupBy from 'lodash/groupBy';
import pick from 'lodash/pick';
import omit from 'lodash/omit';
import merge from 'lodash/merge';
import cloneDeep from 'lodash/cloneDeep';
import has from 'lodash/has';
import camelCase from 'lodash/camelCase';
import snakeCase from 'lodash/snakeCase';
import kebabCase from 'lodash/kebabCase';
import startCase from 'lodash/startCase';
import truncate from 'lodash/truncate';
import clamp from 'lodash/clamp';
import random from 'lodash/random';
import debounce from 'lodash/debounce';
import throttle from 'lodash/throttle';
import memoize from 'lodash/memoize';

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
};

export default utils;



----------------

  import utils from '@your-workspace/utils';

console.log('Array Utilities:');
const numbers = [1, 2, 3, 4, 5, 5, 6];
const objects = [
  { id: 1, name: 'Alice', age: 30 },
  { id: 2, name: 'Bob', age: 25 },
  { id: 3, name: 'Charlie', age: 35 },
  { id: 4, name: 'David', age: 28 }
];

console.log('chunk:', utils.chunk(numbers, 2));
console.log('uniq:', utils.uniq(numbers));
console.log('flatten:', utils.flatten([[1, 2], [3, 4], [5, [6, 7]]]));
console.log('intersection:', utils.intersection([1, 2, 3], [2, 3, 4], [3, 4, 5]));
console.log('difference:', utils.difference([1, 2, 3, 4, 5], [2, 4], [1, 5]));
console.log('filterArray:', utils.filterArray(objects, obj => obj.age > 28));
console.log('findInArray:', utils.findInArray(objects, obj => obj.name === 'Bob'));
console.log('mapArray:', utils.mapArray(objects, obj => ({ ...obj, ageNextYear: obj.age + 1 })));
console.log('sortArray:', utils.sortArray(objects, ['age', 'name']));
console.log('groupByArray:', utils.groupByArray(objects, obj => obj.age > 30 ? 'senior' : 'junior'));
console.log('uniqArray:', utils.uniqArray([1, 2, 2, 3, 4, 4, 5]));
console.log('compact:', utils.compact([0, 1, false, 2, '', 3, null, undefined]));
console.log('groupBySize:', utils.groupBySize(numbers, 3));
console.log('shuffle:', utils.shuffle(numbers));

console.log('\nObject Utilities:');
const obj = { a: 1, b: { c: 2, d: [3, 4] }, e: 5, f: { g: { h: 6 } } };

console.log('pick:', utils.pick(obj, ['a', 'b', 'e']));
console.log('omit:', utils.omit(obj, ['b', 'f']));
console.log('merge:', utils.merge({ a: 1, b: { c: 2 } }, { b: { d: 3 }, e: 4 }));
console.log('cloneDeep:', utils.cloneDeep(obj));
console.log('has:', utils.has(obj, 'b.c'));
console.log('get:', utils.get(obj, 'b.d[1]', 'default'));
console.log('invert:', utils.invert({ a: '1', b: '2', c: '3' }));

console.log('\nString Utilities:');
const str = 'The quick brown fox jumps over the lazy dog';

console.log('camelCase:', utils.camelCase(str));
console.log('snakeCase:', utils.snakeCase(str));
console.log('kebabCase:', utils.kebabCase(str));
console.log('startCase:', utils.startCase(str));
console.log('truncate:', utils.truncate(str, { length: 20 }));
console.log('capitalize:', utils.capitalize('hello world'));
console.log('reverse:', utils.reverse(str));
console.log('isPalindrome:', utils.isPalindrome('A man a plan a canal Panama'));

console.log('\nNumber Utilities:');

console.log('clamp:', utils.clamp(15, 0, 10));
console.log('random:', utils.random(1, 100));
console.log('round:', utils.round(3.14159, 2));
console.log('formatCurrency:', utils.formatCurrency(1234567.89, 'EUR', 'de-DE'));

console.log('\nFunction Utilities:');

const expensiveOperation = (n: number) => {
  let result = 0;
  for (let i = 0; i < n; i++) {
    result += Math.sqrt(i);
  }
  return result;
};

const memoizedExpensiveOperation = utils.memoize(expensiveOperation);

console.time('Non-memoized');
console.log('expensiveOperation result:', expensiveOperation(1000000));
console.timeEnd('Non-memoized');

console.time('Memoized (first call)');
console.log('memoizedExpensiveOperation result:', memoizedExpensiveOperation(1000000));
console.timeEnd('Memoized (first call)');

console.time('Memoized (second call)');
console.log('memoizedExpensiveOperation result:', memoizedExpensiveOperation(1000000));
console.timeEnd('Memoized (second call)');

const debouncedFunction = utils.debounce(() => console.log('Debounced function called'), 1000);
debouncedFunction();
debouncedFunction();
debouncedFunction();

const throttledFunction = utils.throttle(() => console.log('Throttled function called'), 1000);
throttledFunction();
throttledFunction();
throttledFunction();

const isEven = (n: number) => n % 2 === 0;
const isOdd = utils.negate(isEven);
console.log('isOdd(3):', isOdd(3));

let count = 0;
const incrementOnce = utils.once(() => ++count);
incrementOnce();
incrementOnce();
incrementOnce();
console.log('count after incrementOnce:', count);

console.log('\nCrypto Utilities:');

console.log('generateGuid:', utils.generateGuid());
console.log('generateGuid with timestamp:', utils.generateGuid(1625097600000)); // July 1, 2021 UTC

console.log('\nCommon Utilities:');
console.log('isEmpty (various types):');
console.log('  Empty array:', utils.isEmpty([]));
console.log('  Empty object:', utils.isEmpty({}));
console.log('  Empty string:', utils.isEmpty(''));
console.log('  Null:', utils.isEmpty(null));
console.log('  Undefined:', utils.isEmpty(undefined));
console.log('  Non-empty array:', utils.isEmpty([1, 2, 3]));
console.log('  Non-empty object:', utils.isEmpty({ a: 1 }));
console.log('  Non-empty string:', utils.isEmpty('hello'));

console.log('isNotEmpty (various types):');
console.log('  Empty array:', utils.isNotEmpty([]));
console.log('  Non-empty array:', utils.isNotEmpty([1, 2, 3]));

console.log('isEmptyDetailed (various types):');
console.log('  NaN:', utils.isEmptyDetailed(NaN));
console.log('  0:', utils.isEmptyDetailed(0));

console.log('isNotEmptyDetailed (various types):');
console.log('  NaN:', utils.isNotEmptyDetailed(NaN));
console.log('  0:', utils.isNotEmptyDetailed(0));

console.log('\nPerformance Utilities:');
const [result, executionTime] = utils.measureExecutionTime(() => {
  let sum = 0;
  for (let i = 0; i < 1000000; i++) {
    sum += i;
  }
  return sum;
});
console.log(`Sum of numbers from 0 to 999999: ${result}`);
console.log(`Execution time: ${executionTime.toFixed(2)} ms`);

console.log('\nJSON Utilities:');
const obj1 = { 
  a: 1, 
  b: [1, 2, 3], 
  c: { d: 4, e: { f: 5 } },
  g: new Date('2023-01-01'),
  h: /test/,
  i: null,
  j: undefined,
  k: NaN,
  l: Infinity,
  m: -Infinity,
  n: [{ o: 1 }, { p: 2 }]
};
const obj2 = { 
  a: 1, 
  b: [1, 2, 4], 
  c: { d: 4, e: { f: 6 } }, 
  g: new Date('2023-01-02'),
  h: /test/i,
  i: null,
  j: null,
  k: NaN,
  l: -Infinity,
  m: Infinity,
  n: [{ o: 1 }, { q: 3 }]
};

console.log('compareJSON:', utils.compareJSON(obj1, obj2));
console.log('findJSONDifference:', utils.findJSONDifference(obj1, obj2));
console.log('hasDifference:', utils.hasDifference(obj1, obj2));

// Complex usage examples

console.log('\nComplex Usage Examples:');

// 1. Data processing pipeline
const rawData = [
  { name: 'John Doe', age: '32', salary: '$50000' },
  { name: 'Jane Smith', age: '28', salary: '$60000' },
  { name: 'Bob Johnson', age: '45', salary: '$75000' }
];

const processedData = utils.flowRight([
  (data) => utils.sortArray(data, 'age'),
  (data) => utils.mapArray(data, item => ({
    ...item,
    age: parseInt(item.age),
    salary: parseInt(item.salary.replace('$', '')),
    id: utils.generateGuid()
  })),
  (data) => utils.filterArray(data, item => item.age > 30),
  (data) => utils.mapArray(data, item => ({
    ...item,
    name: utils.startCase(item.name),
    formattedSalary: utils.formatCurrency(item.salary, 'USD')
  }))
])(rawData);

console.log('Processed data:', processedData);

// 2. Deep object manipulation
const complexObject = {
  user: {
    personal: {
      name: 'John Doe',
      age: 32,
      address: {
        street: '123 Main St',
        city: 'Anytown',
        country: 'USA'
      }
    },
    professional: {
      title: 'Software Engineer',
      experience: 5,
      skills: ['JavaScript', 'TypeScript', 'React', 'Node.js']
    }
  },
  company: {
    name: 'Tech Corp',
    founded: 2010,
    employees: 500
  }
};

const updateNestedProperty = (obj: any, path: string, value: any) => {
  const parts = path.split('.');
  const lastPart = parts.pop();
  const target = utils.get(obj, parts.join('.'), {});
  if (lastPart) {
    target[lastPart] = value;
  }
  return obj;
};

const updatedObject = utils.flowRight([
  (obj) => updateNestedProperty(obj, 'user.personal.age', 33),
  (obj) => updateNestedProperty(obj, 'user.professional.skills', [...obj.user.professional.skills, 'Python']),
  (obj) => updateNestedProperty(obj, 'company.employees', obj.company.employees + 50),
  (obj) => utils.omit(obj.user.personal, ['address']),
  (obj) => ({ ...obj, lastUpdated: new Date() })
])(utils.cloneDeep(complexObject));

console.log('Updated complex object:', updatedObject);

// 3. Advanced data analysis
const salesData = [
  { date: '2023-01-01', product: 'A', amount: 100 },
  { date: '2023-01-02', product: 'B', amount: 200 },
  { date: '2023-01-01', product: 'A', amount: 150 },
  { date: '2023-01-03', product: 'C', amount: 300 },
  { date: '2023-01-02', product: 'B', amount: 250 },
  { date: '2023-01-03', product: 'A', amount: 180 }
];

const analyzeSales = (data: typeof salesData) => {
  const groupedByDate = utils.groupByArray(data, 'date');
  const groupedByProduct = utils.groupByArray(data, 'product');

  const dailyTotals = utils.mapArray(Object.entries(groupedByDate), ([date, sales]) => ({
    date,
    total: utils.sumBy(sales, 'amount')
  }));

  const productTotals = utils.mapArray(Object.entries(groupedByProduct), ([product, sales]) => ({
    product,
    total: utils.sumBy(sales, 'amount'),
    average: utils.meanBy(sales, 'amount')
  }));

  const overallTotal = utils.sumBy(data, 'amount');
  const overallAverage = utils.meanBy(data, 'amount');

  return {
    dailyTotals: utils.sortArray(dailyTotals, 'date'),
    productTotals: utils.sortArray(productTotals, (item) => -item.total),
    overallTotal,
    overallAverage
  };
};

console.log('Sales analysis:', analyzeSales(salesData));
