// hover-wrapper.component.ts
import { Component, Input, ViewChild, ElementRef, OnDestroy } from '@angular/core';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'hover-wrapper',
  standalone: true,
  imports: [CommonModule],
  template: `
    <div 
      class="relative inline-block"
      (mouseenter)="handleMouseEnter()"
      (mouseleave)="handleMouseLeave()"
      #wrapperElement
    >
      <!-- Replace 'third-party-tag' with your actual web component tag -->
      <third-party-tag></third-party-tag>
    </div>
  `,
  host: {
    'class': 'block'
  }
})
export class HoverWrapperComponent implements OnDestroy {
  @Input() hoverDelay = 200;
  @Input() closeDelay = 300;
  @ViewChild('wrapperElement') wrapperElement!: ElementRef;

  private openTimeout: ReturnType<typeof setTimeout> | null = null;
  private closeTimeout: ReturnType<typeof setTimeout> | null = null;

  handleMouseEnter(): void {
    if (this.closeTimeout) {
      clearTimeout(this.closeTimeout);
      this.closeTimeout = null;
    }

    this.openTimeout = setTimeout(() => {
      // Replace 'third-party-tag' with your actual web component tag
      const element = this.wrapperElement.nativeElement.querySelector('third-party-tag');
      if (element) {
        element.click();
      }
    }, this.hoverDelay);
  }

  handleMouseLeave(): void {
    if (this.openTimeout) {
      clearTimeout(this.openTimeout);
      this.openTimeout = null;
    }

    this.closeTimeout = setTimeout(() => {
      const element = this.wrapperElement.nativeElement.querySelector('third-party-tag');
      if (element) {
        element.click();
      }
    }, this.closeDelay);
  }

  ngOnDestroy(): void {
    if (this.openTimeout) {
      clearTimeout(this.openTimeout);
    }
    if (this.closeTimeout) {
      clearTimeout(this.closeTimeout);
    }
  }
}
