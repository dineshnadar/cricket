// filter-utils.ts

type Condition = {
    [key: string]: any;
};

type FilterConditions = Condition | { AND: FilterConditions[] } | { OR: FilterConditions[] };

export function filterObjects<T>(data: T[], conditions: FilterConditions): T[] {
    return data.filter(item => {
        const evaluate = (item: T, cond: FilterConditions): boolean => {
            if (Array.isArray(cond)) {
                return cond.every(c => evaluate(item, c));
            }
            if ('OR' in cond) {
                return cond.OR.some(c => evaluate(item, c));
            }
            if ('AND' in cond) {
                return cond.AND.every(c => evaluate(item, c));
            }
            return Object.entries(cond).every(([key, value]) => {
                if (typeof item === 'object' && item !== null) {
                    return (item as any)[key] === value;
                }
                return false;
            });
        };
        
        return evaluate(item, conditions);
    });
}

// filter.component.ts

import { Component } from '@angular/core';
import { filterObjects } from './filter-utils';

interface Product {
    id: string;
    name: string;
    category: string;
    price: number;
    inStock: boolean;
}

@Component({
    selector: 'app-filter',
    template: `
        <h2>Product Filter</h2>
        <button (click)="applyFilter('inStock')">Show In Stock</button>
        <button (click)="applyFilter('category')">Show Electronics</button>
        <button (click)="applyFilter('price')">Show Price < 500</button>
        <button (click)="applyFilter('complex')">Complex Filter</button>
        <button (click)="resetFilter()">Reset</button>
        
        <div *ngIf="filteredProducts.length > 0">
            <h3>Filtered Products ({{ filteredProducts.length }})</h3>
            <ul>
                <li *ngFor="let product of filteredProducts">
                    {{ product.name }} - ${{ product.price }} 
                    ({{ product.category }}, {{ product.inStock ? 'In Stock' : 'Out of Stock' }})
                </li>
            </ul>
        </div>
        <div *ngIf="filteredProducts.length === 0">
            <p>No products match the current filter.</p>
        </div>
    `
})
export class FilterComponent {
    private products: Product[] = [
        { id: '1', name: 'Laptop', category: 'Electronics', price: 999, inStock: true },
        { id: '2', name: 'Smartphone', category: 'Electronics', price: 699, inStock: true },
        { id: '3', name: 'Headphones', category: 'Electronics', price: 199, inStock: false },
        { id: '4', name: 'Book', category: 'Books', price: 15, inStock: true },
        { id: '5', name: 'Desk Chair', category: 'Furniture', price: 250, inStock: true },
        { id: '6', name: 'Coffee Maker', category: 'Appliances', price: 89, inStock: false },
        { id: '7', name: 'Gaming Console', category: 'Electronics', price: 499, inStock: true },
        { id: '8', name: 'Tablet', category: 'Electronics', price: 349, inStock: false },
    ];

    filteredProducts: Product[] = [...this.products];

    applyFilter(filterType: string): void {
        switch (filterType) {
            case 'inStock':
                this.filteredProducts = filterObjects(this.products, { inStock: true });
                break;
            case 'category':
                this.filteredProducts = filterObjects(this.products, { category: 'Electronics' });
                break;
            case 'price':
                this.filteredProducts = filterObjects(this.products, (item: Product) => item.price < 500);
                break;
            case 'complex':
                this.filteredProducts = filterObjects(this.products, {
                    AND: [
                        { category: 'Electronics' },
                        { inStock: true },
                        (item: Product) => item.price >= 300
                    ]
                });
                break;
            default:
                this.resetFilter();
        }
    }

    resetFilter(): void {
        this.filteredProducts = [...this.products];
    }
}

// app.module.ts

import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { FilterComponent } from './filter.component';

@NgModule({
    declarations: [FilterComponent],
    imports: [BrowserModule],
    bootstrap: [FilterComponent]
})
export class AppModule { }
