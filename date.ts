// optimized-filter-utils.ts

type Condition = Record<string, any>;
type FilterConditions = Condition | { AND: FilterConditions[] } | { OR: FilterConditions[] };

function filterObjects<T>(data: T[], conditions: FilterConditions, nestedPath: string = ''): T[] {
    const pathParts = nestedPath.split('.');

    function evaluateConditions(item: T, cond: FilterConditions): boolean {
        if (Array.isArray(cond)) {
            return cond.every(c => evaluateConditions(item, c));
        }
        if ('OR' in cond) {
            return cond.OR.some(c => evaluateConditions(item, c));
        }
        if ('AND' in cond) {
            return cond.AND.every(c => evaluateConditions(item, c));
        }
        return Object.entries(cond).every(([key, value]) => {
            const itemValue = getNestedValue(item, key);
            return Array.isArray(itemValue) 
                ? itemValue.some(v => compareValues(v, value))
                : compareValues(itemValue, value);
        });
    }

    function getNestedValue(obj: any, key: string): any {
        let current = obj;
        for (const part of pathParts) {
            if (current[part] === undefined) return undefined;
            current = current[part];
        }
        return current[key];
    }

    function compareValues(a: any, b: any): boolean {
        if (typeof b === 'function') {
            return b(a);
        }
        return a === b;
    }

    return data.filter(item => evaluateConditions(item, conditions));
}

// Example Data Structures
const flatData = [
    { id: 1, name: 'John', age: 30, city: 'New York', tags: ['developer', 'javascript'] },
    { id: 2, name: 'Jane', age: 25, city: 'Los Angeles', tags: ['designer', 'ui/ux'] },
    { id: 3, name: 'Bob', age: 40, city: 'Chicago', tags: ['manager', 'agile'] },
    { id: 4, name: 'Alice', age: 35, city: 'New York', tags: ['developer', 'python'] }
];

const nestedData = [
    {
        id: 1,
        name: 'Product A',
        details: {
            price: 100,
            category: 'Electronics',
            specs: [
                { key: 'color', value: 'black' },
                { key: 'weight', value: '200g' }
            ]
        },
        inStock: true
    },
    {
        id: 2,
        name: 'Product B',
        details: {
            price: 150,
            category: 'Clothing',
            specs: [
                { key: 'size', value: 'M' },
                { key: 'color', value: 'red' }
            ]
        },
        inStock: false
    }
];

// Usage Examples

console.log("1. Simple flat data filter:");
console.log(filterObjects(flatData, { city: 'New York' }).map(item => item.name));

console.log("\n2. Complex flat data filter with AND/OR:");
console.log(filterObjects(flatData, {
    OR: [
        { AND: [{ city: 'New York' }, { age: (age: number) => age > 30 }] },
        { tags: 'designer' }
    ]
}).map(item => item.name));

console.log("\n3. Nested data filter:");
console.log(filterObjects(nestedData, { category: 'Electronics' }, 'details').map(item => item.name));

console.log("\n4. Complex nested data filter:");
console.log(filterObjects(nestedData, {
    AND: [
        { price: (price: number) => price > 100 },
        { specs: { key: 'color', value: 'red' } }
    ]
}, 'details').map(item => item.name));

console.log("\n5. Filter with custom function:");
console.log(filterObjects(flatData, {
    age: (age: number) => age >= 30 && age < 40
}).map(item => item.name));

console.log("\n6. Filter array values:");
console.log(filterObjects(flatData, { tags: 'developer' }).map(item => item.name));

console.log("\n7. Complex filter with multiple nested conditions:");
console.log(filterObjects(nestedData, {
    OR: [
        {
            AND: [
                { category: 'Electronics' },
                { specs: { key: 'color', value: 'black' } }
            ]
        },
        {
            AND: [
                { price: (price: number) => price > 100 },
                { inStock: true }
            ]
        }
    ]
}, 'details').map(item => item.name));
