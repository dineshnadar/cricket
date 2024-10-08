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
        return Object.entries(cond).every(([key, value]) => {
            const itemValue = getNestedValue(item, key);
            return compareValues(itemValue, value);
        });
    }

    function getNestedValue(obj: any, key: string): any {
        let current = obj;
        for (const part of pathParts) {
            if (current[part] === undefined) return undefined;
            current = current[part];
        }
        return current[key] !== undefined ? current[key] : current;
    }

    function compareValues(a: any, b: any): boolean {
        if (typeof b === 'function') {
            return b(a);
        }
        if (Array.isArray(a)) {
            return a.some(item => compareValues(item, b));
        }
        if (typeof a === 'object' && a !== null && typeof b === 'object' && b !== null) {
            return Object.entries(b).every(([k, v]) => compareValues(a[k], v));
        }
        return a === b;
    }

    return data.filter(item => evaluateConditions(item, conditions));
}
