generatePDF() {
  // Create a flattened version of your shadow DOM content
  const shadowHost = this.contentToCapture.element.nativeElement;
  const flattenedContent = this.flattenShadowDOM(shadowHost);
  
  // Temporarily append to document
  document.body.appendChild(flattenedContent);
  
  // Generate PDF
  this.pdfService.generatePDF(flattenedContent, {
    // options...
  }).subscribe({
    complete: () => {
      // Clean up
      document.body.removeChild(flattenedContent);
    }
  });
}

private flattenShadowDOM(node: Node): HTMLElement {
  // Create a new element to hold the flattened content
  const container = document.createElement('div');
  
  // Handle shadow roots
  if (node instanceof HTMLElement && node.shadowRoot) {
    // Process all nodes in the shadow root
    node.shadowRoot.childNodes.forEach(child => {
      const flattened = this.flattenShadowDOM(child);
      if (flattened) {
        container.appendChild(flattened);
      }
    });
  } 
  // Handle regular nodes
  else if (node.nodeType === Node.ELEMENT_NODE) {
    // Clone the node
    const clone = (node as HTMLElement).cloneNode(false) as HTMLElement;
    container.appendChild(clone);
    
    // Process children
    node.childNodes.forEach(child => {
      const flattened = this.flattenShadowDOM(child);
      if (flattened) {
        clone.appendChild(flattened);
      }
    });
    
    return clone;
  } 
  // Handle text nodes
  else if (node.nodeType === Node.TEXT_NODE) {
    return document.createTextNode(node.nodeValue || '') as unknown as HTMLElement;
  }
  
  return container;
}

// pdf.service.ts
import { Injectable } from '@angular/core';
import html2canvas from 'html2canvas';
import { jsPDF } from 'jspdf';
import { Observable, from } from 'rxjs';
import { finalize } from 'rxjs/operators';

export interface PDFGenerationOptions {
  headerTitle?: string;
  customerName?: string;
  includeFooter?: boolean;
  includeHeader?: boolean;
  filename?: string;
  compression?: boolean;
  imageQuality?: number;
  imageType?: 'png' | 'jpeg';
  scaleFactor?: number;
}

@Injectable({
  providedIn: 'root'
})
export class PdfService {
  private defaultOptions: PDFGenerationOptions = {
    headerTitle: 'COMPANY NAME',
    customerName: 'Customer',
    includeFooter: true,
    includeHeader: true,
    filename: 'document.pdf',
    compression: true,
    imageQuality: 0.92,
    imageType: 'jpeg',
    scaleFactor: 2
  };

  constructor() {}

  generatePDF(element: HTMLElement, options?: PDFGenerationOptions): Observable<void> {
    const mergedOptions = { ...this.defaultOptions, ...options };
    
    return from(this.generatePDFAsync(element, mergedOptions));
  }
  
  generateMultiSectionPDF(sections: HTMLElement[], options?: PDFGenerationOptions): Observable<void> {
    const mergedOptions = { ...this.defaultOptions, ...options };
    
    return from(this.generateMultiSectionPDFAsync(sections, mergedOptions))
      .pipe(
        finalize(() => console.log('PDF generation completed'))
      );
  }

  private async generatePDFAsync(element: HTMLElement, options: PDFGenerationOptions): Promise<void> {
    try {
      const pdf = new jsPDF({
        orientation: 'p',
        unit: 'px',
        format: 'a4',
        compress: options.compression
      });
      
      const pageWidth = pdf.internal.pageSize.getWidth();
      const pageHeight = pdf.internal.pageSize.getHeight();
      
      const canvas = await this.renderElementToCanvas(element, options.scaleFactor || 2);
      const imgData = canvas.toDataURL(`image/${options.imageType}`, options.imageQuality);
      
      const imgProps = pdf.getImageProperties(imgData);
      const contentWidth = pageWidth;
      const contentHeight = (imgProps.height * contentWidth) / imgProps.width;
      
      await this.addContentToPDF(pdf, imgData, contentWidth, contentHeight, options);
      
      pdf.save(options.filename);
    } catch (error) {
      console.error('Error generating PDF:', error);
      throw error;
    }
  }
  
  private async generateMultiSectionPDFAsync(sections: HTMLElement[], options: PDFGenerationOptions): Promise<void> {
    const pdf = new jsPDF({
      orientation: 'p',
      unit: 'px',
      format: 'a4',
      compress: options.compression
    });
    
    const pageWidth = pdf.internal.pageSize.getWidth();
    const pageHeight = pdf.internal.pageSize.getHeight();
    
    let yPosition = options.includeHeader ? 40 : 10;
    let pageNum = 1;
    
    if (options.includeHeader) {
      this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                          new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME');
    }
    
    for (let i = 0; i < sections.length; i++) {
      const currentSection = sections[i];
      
      const canvas = await this.renderElementToCanvas(currentSection, options.scaleFactor || 2);
      const imgData = canvas.toDataURL(`image/${options.imageType}`, options.imageQuality);
      
      const imgProps = pdf.getImageProperties(imgData);
      const contentWidth = pageWidth - 20;
      const contentHeight = (imgProps.height * contentWidth) / imgProps.width;
      
      if (yPosition + contentHeight > pageHeight - (options.includeFooter ? 30 : 10)) {
        if (options.includeFooter) {
          this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum);
        }
        
        pdf.addPage();
        pageNum++;
        yPosition = options.includeHeader ? 40 : 10;
        
        if (options.includeHeader) {
          this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                              new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME');
        }
      }
      
      pdf.addImage(imgData, options.imageType?.toUpperCase() || 'JPEG', 10, yPosition, contentWidth, contentHeight);
      
      yPosition += contentHeight + 10;
    }
    
    if (options.includeFooter) {
      this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum);
    }
    
    pdf.save(options.filename);
  }
  
  private async renderElementToCanvas(element: HTMLElement, scaleFactor: number = 2): Promise<HTMLCanvasElement> {
    const originalDisplay = element.style.display;
    const originalPosition = element.style.position;
    const originalTransform = element.style.transform;
    
    try {
      element.style.display = 'block';
      element.style.position = 'relative';
      element.style.transform = 'translate3d(0,0,0)';
      
      return await html2canvas(element, {
        scale: scaleFactor,
        logging: false,
        allowTaint: true,
        useCORS: true,
        backgroundColor: '#ffffff',
        scrollX: 0,
        scrollY: -window.scrollY,
        windowWidth: element.scrollWidth,
        windowHeight: element.scrollHeight,
        onclone: (doc) => {
          const clonedElement = doc.querySelector(`#${element.id}`) as HTMLElement;
          if (clonedElement) {
          }
          return Promise.resolve();
        }
      });
    } finally {
      element.style.display = originalDisplay;
      element.style.position = originalPosition;
      element.style.transform = originalTransform;
    }
  }
  
  private async addContentToPDF(
    pdf: jsPDF,
    imgData: string,
    contentWidth: number,
    contentHeight: number,
    options: PDFGenerationOptions
  ): Promise<void> {
    const pageWidth = pdf.internal.pageSize.getWidth();
    const pageHeight = pdf.internal.pageSize.getHeight();
    const customerName = options.customerName || 'Customer';
    const currentDate = new Date().toLocaleDateString();
    const headerHeight = options.includeHeader ? 40 : 0;
    const footerHeight = options.includeFooter ? 30 : 0;
    const availableHeight = pageHeight - headerHeight - footerHeight;
    
    let heightLeft = contentHeight;
    let position = headerHeight;
    let pageNum = 1;
    
    if (options.includeHeader) {
      this.addHeaderToPDF(pdf, pageWidth, customerName, currentDate, options.headerTitle);
    }
    
    pdf.addImage(imgData, options.imageType?.toUpperCase() || 'JPEG', 0, position, contentWidth, contentHeight);
    
    if (options.includeFooter) {
      this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum);
    }
    
    heightLeft -= availableHeight;
    
    while (heightLeft > 0) {
      pdf.addPage();
      pageNum++;
      
      if (options.includeHeader) {
        this.addHeaderToPDF(pdf, pageWidth, customerName, currentDate, options.headerTitle);
      }
      
      position = ((heightLeft > availableHeight) ? position - availableHeight : position - heightLeft) - headerHeight;
      
      pdf.addImage(imgData, options.imageType?.toUpperCase() || 'JPEG', 0, position, contentWidth, contentHeight);
      
      if (options.includeFooter) {
        this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum);
      }
      
      heightLeft -= availableHeight;
    }
  }
  
  private addHeaderToPDF(
    pdf: jsPDF, 
    pageWidth: number, 
    customerName: string, 
    date: string,
    companyName: string = 'COMPANY NAME'
  ): void {
    pdf.setFont('helvetica', 'bold');
    pdf.setFontSize(12);
    
    pdf.setTextColor(50, 50, 150);
    pdf.text(companyName, 10, 20);
    
    pdf.setTextColor(100, 100, 100);
    pdf.setFontSize(10);
    pdf.text(`Customer: ${customerName}`, pageWidth - 10, 15, { align: 'right' });
    pdf.text(`Date: ${date}`, pageWidth - 10, 25, { align: 'right' });
    
    pdf.setDrawColor(200, 200, 200);
    pdf.line(10, 30, pageWidth - 10, 30);
  }
  
  private addFooterToPDF(pdf: jsPDF, pageWidth: number, pageHeight: number, pageNum: number): void {
    pdf.setFont('helvetica', 'normal');
    pdf.setFontSize(9);
    pdf.setTextColor(100, 100, 100);
    
    pdf.text('Generated with Angular PDF Service', 10, pageHeight - 15);
    
    pdf.text(`Page ${pageNum}`, pageWidth - 10, pageHeight - 15, { align: 'right' });
    
    pdf.setDrawColor(200, 200, 200);
    pdf.line(10, pageHeight - 25, pageWidth - 10, pageHeight - 25);
  }
}

// pdf-generator.component.ts
import { Component, OnInit, ViewChild, ElementRef } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { PdfService } from '../services/pdf.service';
import { finalize } from 'rxjs/operators';

interface Product {
  name: string;
  quantity: number;
  price: number;
  total: number;
}

@Component({
  selector: 'app-pdf-generator',
  templateUrl: './pdf-generator.component.html',
  styleUrls: ['./pdf-generator.component.scss']
})
export class PdfGeneratorComponent implements OnInit {
  @ViewChild('contentToCapture') contentToCapture!: ElementRef;
  @ViewChild('customerFormSection') customerFormSection!: ElementRef;
  @ViewChild('productsTableSection') productsTableSection!: ElementRef;
  
  customerForm: FormGroup;
  products: Product[] = [];
  isGenerating = false;
  
  constructor(
    private fb: FormBuilder,
    private pdfService: PdfService
  ) {
    this.customerForm = this.fb.group({
      name: ['John Doe'],
      email: ['john@example.com'],
      category: ['premium']
    });
    
    this.products = [
      { name: 'Laptop', quantity: 2, price: 800, total: 1600 },
      { name: 'Mouse', quantity: 5, price: 20, total: 100 },
      { name: 'Keyboard', quantity: 3, price: 50, total: 150 },
    ];
  }

  ngOnInit(): void {}

  generateSinglePDF(): void {
    if (this.isGenerating) return;
    this.isGenerating = true;
    
    const customerName = this.customerForm.get('name')?.value || 'Customer';
    
    this.pdfService.generatePDF(this.contentToCapture.nativeElement, {
      customerName: customerName,
      filename: `${customerName.replace(/\s+/g, '_')}_order.pdf`,
      imageQuality: 0.9,
      scaleFactor: 2
    })
    .pipe(
      finalize(() => this.isGenerating = false)
    )
    .subscribe({
      error: (err) => console.error('PDF generation failed:', err)
    });
  }
  
  generateMultiSectionPDF(): void {
    if (this.isGenerating) return;
    this.isGenerating = true;
    
    const customerName = this.customerForm.get('name')?.value || 'Customer';
    
    const sections = [
      this.customerFormSection.nativeElement,
      this.productsTableSection.nativeElement
    ];
    
    this.pdfService.generateMultiSectionPDF(sections, {
      customerName: customerName,
      filename: `${customerName.replace(/\s+/g, '_')}_order.pdf`,
      imageQuality: 0.9,
      scaleFactor: 2
    })
    .pipe(
      finalize(() => this.isGenerating = false)
    )
    .subscribe({
      error: (err) => console.error('PDF generation failed:', err)
    });
  }
}

// pdf-generator.component.html
<div class="container">
  <h1>PDF Generator</h1>
  
  <form [formGroup]="customerForm">
    <h2>Customer Information</h2>
    <div class="form-group">
      <label for="name">Name:</label>
      <input type="text" id="name" formControlName="name">
    </div>
    <div class="form-group">
      <label for="email">Email:</label>
      <input type="email" id="email" formControlName="email">
    </div>
    <div class="form-group">
      <label for="category">Category:</label>
      <select id="category" formControlName="category">
        <option value="premium">Premium</option>
        <option value="standard">Standard</option>
        <option value="basic">Basic</option>
      </select>
    </div>
  </form>

  <div #contentToCapture>
    <div #customerFormSection id="customer-form-section">
      <form [formGroup]="customerForm">
        <h2>Customer Information</h2>
        <div class="form-group">
          <label for="pdf-name">Name:</label>
          <input type="text" id="pdf-name" [value]="customerForm.get('name')?.value">
        </div>
        <div class="form-group">
          <label for="pdf-email">Email:</label>
          <input type="email" id="pdf-email" [value]="customerForm.get('email')?.value">
        </div>
        <div class="form-group">
          <label for="pdf-category">Category:</label>
          <select id="pdf-category" [value]="customerForm.get('category')?.value">
            <option value="premium">Premium</option>
            <option value="standard">Standard</option>
            <option value="basic">Basic</option>
          </select>
        </div>
      </form>
    </div>

    <div #productsTableSection id="products-table-section">
      <h2>Order Details</h2>
      <table>
        <thead>
          <tr>
            <th>Product</th>
            <th>Quantity</th>
            <th>Price</th>
            <th>Total</th>
          </tr>
        </thead>
        <tbody>
          <tr *ngFor="let product of products">
            <td>{{ product.name }}</td>
            <td>{{ product.quantity }}</td>
            <td>${{ product.price }}</td>
            <td>${{ product.total }}</td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>

  <div class="button-group">
    <button [disabled]="isGenerating" (click)="generateSinglePDF()">
      {{ isGenerating ? 'Generating...' : 'Generate Single PDF' }}
    </button>
    
    <button [disabled]="isGenerating" (click)="generateMultiSectionPDF()">
      {{ isGenerating ? 'Generating...' : 'Generate Multi-Section PDF' }}
    </button>
  </div>
</div>
