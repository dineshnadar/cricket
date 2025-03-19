// pdf.service.ts
import { Injectable } from '@angular/core';
import { jsPDF } from 'jspdf';
import * as domtoimage from 'dom-to-image';
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
    imageType: 'png',
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
      
      const imgData = await this.renderElementToImage(element, options);
      
      const img = new Image();
      img.src = imgData;
      
      await new Promise<void>((resolve) => {
        img.onload = () => {
          const contentWidth = pageWidth;
          const contentHeight = (img.height * contentWidth) / img.width;
          
          this.addContentToPDF(pdf, imgData, contentWidth, contentHeight, options);
          
          pdf.save(options.filename);
          resolve();
        };
      });
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
      
      const imgData = await this.renderElementToImage(currentSection, options);
      
      const img = new Image();
      img.src = imgData;
      
      await new Promise<void>((resolve) => {
        img.onload = () => {
          const contentWidth = pageWidth - 20;
          const contentHeight = (img.height * contentWidth) / img.width;
          
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
          
          pdf.addImage(imgData, 'PNG', 10, yPosition, contentWidth, contentHeight);
          
          yPosition += contentHeight + 10;
          resolve();
        };
      });
    }
    
    if (options.includeFooter) {
      this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum);
    }
    
    pdf.save(options.filename);
  }
  
  private async renderElementToImage(element: HTMLElement, options: PDFGenerationOptions): Promise<string> {
    try {
      // Store the original styles
      const originalStyles = {
        transform: element.style.transform,
        transformOrigin: element.style.transformOrigin,
        width: element.style.width,
        height: element.style.height
      };
      
      // Apply optimizations if needed
      if (options.scaleFactor && options.scaleFactor !== 1) {
        element.style.transform = `scale(${options.scaleFactor})`;
        element.style.transformOrigin = 'top left';
      }
      
      // Choose the appropriate dom-to-image method based on imageType
      let imagePromise: Promise<string>;
      if (options.imageType === 'jpeg') {
        imagePromise = domtoimage.toJpeg(element, {
          quality: options.imageQuality || 0.92,
          bgcolor: '#ffffff',
          height: element.scrollHeight,
          width: element.scrollWidth,
          style: {
            'transform': 'scale(1)',
            'transform-origin': 'top left'
          }
        });
      } else {
        imagePromise = domtoimage.toPng(element, {
          height: element.scrollHeight,
          width: element.scrollWidth,
          style: {
            'transform': 'scale(1)',
            'transform-origin': 'top left'
          }
        });
      }
      
      // Process the image
      const dataUrl = await imagePromise;
      
      // Restore the original styles
      element.style.transform = originalStyles.transform;
      element.style.transformOrigin = originalStyles.transformOrigin;
      element.style.width = originalStyles.width;
      element.style.height = originalStyles.height;
      
      return dataUrl;
    } catch (error) {
      console.error('Error rendering element to image:', error);
      throw error;
    }
  }
  
  private addContentToPDF(
    pdf: jsPDF,
    imgData: string,
    contentWidth: number,
    contentHeight: number,
    options: PDFGenerationOptions
  ): void {
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
    
    pdf.addImage(imgData, 'PNG', 0, position, contentWidth, contentHeight);
    
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
      
      pdf.addImage(imgData, 'PNG', 0, position, contentWidth, contentHeight);
      
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
