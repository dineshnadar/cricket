// hover-wrapper.component.ts
import { Component, Input, ViewChild, ElementRef, OnDestroy } from '@angular/core';
import { CommonModule } from '@angular/common';
import { YourClickComponent } from './your-click.component'; // Import your click component

@Component({
  selector: 'hover-wrapper',
  standalone: true,
  imports: [CommonModule, YourClickComponent],
  template: `
    <div 
      class="relative inline-block"
      (mouseenter)="handleMouseEnter()"
      (mouseleave)="handleMouseLeave()"
      #wrapperElement
    >
      <your-click-component 
        #clickComponent
        [yourInput1]="input1"
        [yourInput2]="input2"
        (yourOutput1)="handleOutput1($event)"
        (yourOutput2)="handleOutput2($event)"
      >
      </your-click-component>
    </div>
  `,
  host: {
    'class': 'block'
  }
})
export class HoverWrapperComponent implements OnDestroy {
  @Input() hoverDelay = 200;
  @Input() closeDelay = 300;
  @Input() input1: any; // Add inputs that your click component needs
  @Input() input2: any;

  @ViewChild('clickComponent') clickComponent!: YourClickComponent;
  @ViewChild('wrapperElement') wrapperElement!: ElementRef;

  private openTimeout: ReturnType<typeof setTimeout> | null = null;
  private closeTimeout: ReturnType<typeof setTimeout> | null = null;

  handleMouseEnter(): void {
    if (this.closeTimeout) {
      clearTimeout(this.closeTimeout);
      this.closeTimeout = null;
    }

    this.openTimeout = setTimeout(() => {
      const element = this.wrapperElement.nativeElement.querySelector('button, [role="button"]') as HTMLElement;
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
      const element = this.wrapperElement.nativeElement.querySelector('button, [role="button"]') as HTMLElement;
      if (element) {
        element.click();
      }
    }, this.closeDelay);
  }

  // Handle outputs from your click component
  handleOutput1(event: any): void {
    // Handle output1 from your click component
    console.log('Output1:', event);
  }

  handleOutput2(event: any): void {
    // Handle output2 from your click component
    console.log('Output2:', event);
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
