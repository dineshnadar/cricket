// filter-utils.ts

type Condition = {
    [key: string]: any;
};

type FilterConditions = Condition | { AND: FilterConditions[] } | { OR: FilterConditions[] };

export function filterObjects<T>(
    data: T[],
    conditions: FilterConditions,
    nestedPath: string = ''
): T[] {
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

    return data.filter(item => evaluateConditions(item, conditions));
}

// Usage Examples

// 1. Flat Object Filtering
const flatData = [
    { id: 1, name: 'John', age: 30, city: 'New York' },
    { id: 2, name: 'Jane', age: 25, city: 'Los Angeles' },
    { id: 3, name: 'Bob', age: 40, city: 'Chicago' },
    { id: 4, name: 'Alice', age: 35, city: 'New York' }
];

console.log("1. Flat data - Filter by age:");
console.log(filterObjects(flatData, { age: 30 }));

console.log("\n2. Flat data - Filter by age and city:");
console.log(filterObjects(flatData, { AND: [{ age: 35 }, { city: 'New York' }] }));

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
    },
    {
        name: "Product C",
        details: {
            price: 200,
            inStock: true,
            manufacturer: {
                name: "Company X",
                country: "USA"
            }
        }
    }
];

console.log("\n3. Nested data - Filter by price:");
console.log(filterObjects(nestedData, { price: 150 }, "details"));

console.log("\n4. Nested data - Filter by manufacturer country:");
console.log(filterObjects(nestedData, { country: "USA" }, "details.manufacturer"));

// 3. Array of Objects Filtering
const arrayObjectData = [
    {
        name: "2323",
        ind: [
            { code: "mymaster", value: "Y" },
            { code: "myowner", value: "N" },
            { code: "myu", value: "Z" },
            { code: "upc", value: "C" }
        ]
    },
    {
        name: "23D23",
        ind: [
            { code: "mymaster", value: "Y" },
            { code: "myowner", value: "Y" },
            { code: "myu", value: "X" },
            { code: "upc", value: "D" }
        ]
    }
];

console.log("\n5. Array of objects - Filter by mymaster and myowner:");
console.log(filterObjects(arrayObjectData, { AND: [{ mymaster: "Y" }, { myowner: "N" }] }, "ind"));

// 4. Complex Conditions
const complexData = [
    { id: 1, name: 'John', age: 30, skills: ['JavaScript', 'Python'], active: true },
    { id: 2, name: 'Jane', age: 25, skills: ['Java', 'C++'], active: false },
    { id: 3, name: 'Bob', age: 40, skills: ['Python', 'Ruby'], active: true },
    { id: 4, name: 'Alice', age: 35, skills: ['JavaScript', 'TypeScript'], active: true }
];

console.log("\n6. Complex conditions - Filter by age range, skills, and active status:");
console.log(filterObjects(complexData, {
    AND: [
        { OR: [{ age: 30 }, { age: 35 }] },
        (item: any) => item.skills.includes('JavaScript'),
        { active: true }
    ]
}));

// 5. Deeply Nested Structures
const deeplyNestedData = [
    {
        id: 1,
        info: {
            personal: {
                name: 'John',
                age: 30
            },
            professional: {
                skills: ['JavaScript', 'Python'],
                experience: [
                    { company: 'TechCorp', years: 5 },
                    { company: 'WebSolutions', years: 3 }
                ]
            }
        }
    },
    {
        id: 2,
        info: {
            personal: {
                name: 'Jane',
                age: 28
            },
            professional: {
                skills: ['Java', 'C++'],
                experience: [
                    { company: 'DataSystems', years: 4 },
                    { company: 'TechCorp', years: 2 }
                ]
            }
        }
    }
];

console.log("\n7. Deeply nested - Filter by age and company experience:");
console.log(filterObjects(deeplyNestedData, {
    AND: [
        { age: 30 },
        (item: any) => item.info.professional.experience.some((exp: any) => exp.company === 'TechCorp')
    ]
}, "info.personal"));
