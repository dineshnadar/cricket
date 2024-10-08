// filter-utils.ts

type Condition = {
    [key: string]: any;
};

type FilterConditions = Condition | { AND: FilterConditions[] } | { OR: FilterConditions[] };

interface FilterResult<T> {
    data: T[];
    totalCount: number;
    matched: boolean;
    message: string;
}

export function filterObjects<T>(
    data: T[],
    conditions: FilterConditions,
    nestedPath: string = ''
): FilterResult<T> {
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
            const itemValue = getNestedValue(item, nestedPath, key);
            return itemValue === value;
        });
    }

    function getNestedValue(obj: any, path: string, key: string): any {
        const fullPath = path ? `${path}.${key}` : key;
        return fullPath.split('.').reduce((current, part) => {
            if (Array.isArray(current)) {
                return current.find(item => item.code === part)?.value;
            }
            return current && current[part] !== undefined ? current[part] : undefined;
        }, obj);
    }

    const filteredData = data.filter(item => evaluateConditions(item, conditions));

    return {
        data: filteredData,
        totalCount: filteredData.length,
        matched: filteredData.length > 0,
        message: filteredData.length > 0 
            ? `Found ${filteredData.length} matching item(s).`
            : "No items match the given conditions."
    };
}

// Usage Examples

// 1. Flat Object Filtering
const flatData = [
    { id: 1, name: 'John', age: 30, city: 'New York' },
    { id: 2, name: 'Jane', age: 25, city: 'Los Angeles' },
    { id: 3, name: 'Bob', age: 40, city: 'Chicago' },
    { id: 4, name: 'Alice', age: 35, city: 'New York' }
];

console.log("1. Flat data - Filter by age (match):");
console.log(filterObjects(flatData, { age: 30 }));

console.log("\n2. Flat data - Filter by non-existent age (no match):");
console.log(filterObjects(flatData, { age: 50 }));

// 2. Nested Object Filtering
const nestedData = [
    {
        name: "Product A",
        details: {
            price: 100,
            inStock: true,
            manufacturer: {
                name: "Company X",
                country: "USA"
            }
        }
    },
    {
        name: "Product B",
        details: {
            price: 150,
            inStock: false,
            manufacturer: {
                name: "Company Y",
                country: "Canada"
            }
        }
    }
];

console.log("\n3. Nested data - Filter by price (match):");
console.log(filterObjects(nestedData, { price: 150 }, "details"));

console.log("\n4. Nested data - Filter by non-existent country (no match):");
console.log(filterObjects(nestedData, { country: "Germany" }, "details.manufacturer"));

// 3. Complex Conditions
const complexData = [
    { id: 1, name: 'John', age: 30, skills: ['JavaScript', 'Python'], active: true },
    { id: 2, name: 'Jane', age: 25, skills: ['Java', 'C++'], active: false },
    { id: 3, name: 'Bob', age: 40, skills: ['Python', 'Ruby'], active: true },
    { id: 4, name: 'Alice', age: 35, skills: ['JavaScript', 'TypeScript'], active: true }
];

console.log("\n5. Complex conditions - Filter with matches:");
console.log(filterObjects(complexData, {
    AND: [
        { OR: [{ age: 30 }, { age: 35 }] },
        (item: any) => item.skills.includes('JavaScript'),
        { active: true }
    ]
}));

console.log("\n6. Complex conditions - Filter with no matches:");
console.log(filterObjects(complexData, {
    AND: [
        { age: 50 },
        (item: any) => item.skills.includes('Java'),
        { active: false }
    ]
}));
