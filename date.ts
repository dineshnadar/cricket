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
        if (Array.isArray(current)) {
            return current.map(item => item[key]);
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
