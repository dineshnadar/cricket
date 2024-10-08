type Condition = Record<string, any>;
type FilterConditions = Condition | { AND: FilterConditions[] } | { OR: FilterConditions[] } | FilterConditions[];

function filterObjects<T extends Record<string, any>>(
    data: T[],
    conditions: FilterConditions,
    nestedPath: string = ''
): T[] {
    const pathParts = nestedPath ? nestedPath.split('.') : [];

    function evaluateConditions(item: any, cond: FilterConditions): boolean {
        if (Array.isArray(cond)) {
            return cond.some(c => evaluateConditions(item, c));
        }
        if ('AND' in cond) {
            return cond.AND.every(c => evaluateConditions(item, c));
        }
        if ('OR' in cond) {
            return cond.OR.some(c => evaluateConditions(item, c));
        }
        const nestedValue = getNestedValue(item);
        if (Array.isArray(nestedValue)) {
            return nestedValue.some(obj => 
                Object.entries(cond).every(([key, value]) => compareValues(obj[key], value))
            );
        }
        return Object.entries(cond).every(([key, value]) => compareValues(nestedValue[key], value));
    }

    function getNestedValue(obj: any): any {
        return pathParts.reduce((current, part) => current && current[part], obj);
    }

    function compareValues(a: any, b: any): boolean {
        if (typeof b === 'function') {
            return b(a);
        }
        if (Array.isArray(a)) {
            return a.includes(b);
        }
        return a === b;
    }

    return data.filter(item => evaluateConditions(item, conditions));
}
