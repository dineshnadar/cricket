import { 
  Injectable, 
  Signal, 
  signal, 
  computed, 
  ElementRef
} from '@angular/core';
import { DomSanitizer, SafeHtml } from '@angular/platform-browser';

// Default configuration for tooltips
export const DEFAULT_TOOLTIP_CONFIG = {
  delay: 200,
  position: 'top' as const,
  interactive: true
};

// Tooltip Configuration Interface
export interface TooltipConfig {
  message: string | SafeHtml;
  position?: 'top' | 'bottom' | 'left' | 'right';
  delay?: number;
  interactive?: boolean;
}

// Tooltip Service
@Injectable({
  providedIn: 'root'
})
export class TooltipService {
  // Sanitize HTML method
  sanitizeHtml(html: string): SafeHtml {
    return this.domSanitizer.bypassSecurityTrustHtml(html);
  }

  // Private signal for tooltip state management
  private tooltipState = signal({
    isVisible: false,
    content: '' as string | SafeHtml,
    position: DEFAULT_TOOLTIP_CONFIG.position,
    interactive: DEFAULT_TOOLTIP_CONFIG.interactive,
    target: null as ElementRef | null
  });

  // Public computed signals for reactive access
  public readonly isVisible = computed(() => this.tooltipState().isVisible);
  public readonly content = computed(() => this.tooltipState().content);
  public readonly position = computed(() => this.tooltipState().position);
  public readonly interactive = computed(() => this.tooltipState().interactive);
  public readonly target = computed(() => this.tooltipState().target);

  constructor(private domSanitizer: DomSanitizer) {}

  // Show tooltip with given configuration
  public show(config: TooltipConfig, target?: ElementRef): void {
    const sanitizedContent = typeof config.message === 'string' 
      ? config.message 
      : this.domSanitizer.sanitize(SecurityContext.HTML, config.message) || '';

    this.tooltipState.update(state => ({
      isVisible: true,
      content: sanitizedContent,
      position: config.position ?? DEFAULT_TOOLTIP_CONFIG.position,
      interactive: config.interactive ?? DEFAULT_TOOLTIP_CONFIG.interactive,
      target: target ?? state.target
    }));
  }

  // Hide tooltip
  public hide(): void {
    this.tooltipState.update(state => ({
      ...state,
      isVisible: false,
      target: null
    }));
  }
}

////

import { 
  Directive, 
  Input, 
  HostListener, 
  ElementRef, 
  inject, 
  OnDestroy 
} from '@angular/core';
import { TooltipService, TooltipConfig, DEFAULT_TOOLTIP_CONFIG } from './tooltip.model-service';

@Directive({
  selector: '[appTooltip]',
  standalone: true
})
export class TooltipDirective implements OnDestroy {
  // Tooltip configuration input
  @Input('appTooltip') tooltipConfig!: TooltipConfig;

  // Inject dependencies
  private el = inject(ElementRef);
  private tooltipService = inject(TooltipService);

  // Tracking mouse and timer states
  private isMouseInside = false;
  private hoverTimer: number | null = null;
  private hideTimer: number | null = null;

  // Mouse enter event handler
  @HostListener('mouseenter')
  onMouseEnter(): void {
    this.isMouseInside = true;
    this.clearTimers();

    // Set hover timer
    this.hoverTimer = window.setTimeout(() => {
      if (this.isMouseInside) {
        const config = {
          ...DEFAULT_TOOLTIP_CONFIG,
          ...this.tooltipConfig
        };
        this.tooltipService.show(config, this.el);
      }
    }, this.tooltipConfig.delay ?? DEFAULT_TOOLTIP_CONFIG.delay);
  }

  // Mouse leave event handler
  @HostListener('mouseleave')
  onMouseLeave(): void {
    this.isMouseInside = false;
    this.clearTimers();

    // Set hide timer
    this.hideTimer = window.setTimeout(() => {
      if (!this.isMouseInside) {
        this.tooltipService.hide();
      }
    }, this.tooltipConfig.delay ?? DEFAULT_TOOLTIP_CONFIG.delay);
  }

  // Clear existing timers
  private clearTimers(): void {
    if (this.hoverTimer) {
      window.clearTimeout(this.hoverTimer);
      this.hoverTimer = null;
    }
    if (this.hideTimer) {
      window.clearTimeout(this.hideTimer);
      this.hideTimer = null;
    }
  }

  // Cleanup on component destroy
  ngOnDestroy(): void {
    this.clearTimers();
    this.tooltipService.hide();
  }
}

////
import { 
  Component, 
  inject, 
  OnDestroy, 
  ChangeDetectionStrategy 
} from '@angular/core';
import { CommonModule } from '@angular/common';
import { TooltipService } from './tooltip.model-service';
import { TooltipDirective } from './tooltip.directive';

@Component({
  selector: 'app-tooltip',
  standalone: true,
  imports: [CommonModule],
  template: `
    @if (tooltipService.isVisible()) {
      <div 
        class="tooltip" 
        [class]="'tooltip-' + tooltipService.position()"
        [class.tooltip-interactive]="tooltipService.interactive()"
        (mouseenter)="onTooltipMouseEnter()"
        (mouseleave)="onTooltipMouseLeave()"
      >
        <span 
          class="tooltip-content" 
          [class.selectable]="tooltipService.interactive()"
          [innerHTML]="tooltipService.content()"
        ></span>
      </div>
    }
  `,
  styles: [`
    .tooltip {
      position: fixed;
      background-color: #333;
      color: white;
      padding: 5px 10px;
      border-radius: 4px;
      z-index: 1000;
      max-width: 300px;
      word-wrap: break-word;
      transition: opacity 0.3s ease;
    }
    
    .tooltip-interactive {
      pointer-events: auto;
      cursor: text;
    }
    
    .tooltip-content {
      display: block;
      user-select: none;
    }
    
    .selectable {
      user-select: text;
    }
    
    .tooltip-top { 
      transform: translateY(-100%); 
      bottom: 100%; 
    }
    .tooltip-bottom { 
      top: 100%; 
    }
    .tooltip-left { 
      right: 100%; 
      top: 50%;
      transform: translateY(-50%);
    }
    .tooltip-right { 
      left: 100%; 
      top: 50%;
      transform: translateY(-50%);
    }
  `],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class TooltipComponent implements OnDestroy {
  // Inject tooltip service
  protected tooltipService = inject(TooltipService);

  // Tracking mouse state
  private isMouseInside = false;
  private hideTimer: number | null = null;

  // Tooltip mouse enter handler
  onTooltipMouseEnter(): void {
    this.isMouseInside = true;
    
    // Clear hide timer
    if (this.hideTimer) {
      window.clearTimeout(this.hideTimer);
      this.hideTimer = null;
    }
  }

  // Tooltip mouse leave handler
  onTooltipMouseLeave(): void {
    this.isMouseInside = false;
    
    // Set timer to hide tooltip
    this.hideTimer = window.setTimeout(() => {
      if (!this.isMouseInside) {
        this.tooltipService.hide();
      }
    }, 300);
  }

  // Cleanup on component destroy
  ngOnDestroy(): void {
    // Clear any lingering timers
    if (this.hideTimer) {
      window.clearTimeout(this.hideTimer);
      this.hideTimer = null;
    }
  }
}

// Example Usage Component
@Component({
  selector: 'app-example',
  standalone: true,
  imports: [TooltipDirective, TooltipComponent],
  template: `
    <div class="flex space-x-4 p-4">
      <button 
        appTooltip="{
          message: 'Simple string tooltip', 
          position: 'top'
        }"
      >
        String Tooltip
      </button>

      <button 
        appTooltip="{
          message: tooltipService.sanitizeHtml('<strong>HTML</strong> <em>tooltip</em> with <u>formatting</u>'), 
          position: 'bottom'
        }"
      >
        HTML Tooltip via Service
      </button>

      <button 
        appTooltip="{
          message: createSafeHtml('<span style=\'color: red;\'>Custom HTML Tooltip</span>'), 
          position: 'left'
        }"
      >
        HTML Tooltip via Component
      </button>
    </div>
    <app-tooltip />
  `
})
export class ExampleComponent {
  // Inject dependencies
  tooltipService = inject(TooltipService);
  private sanitizer = inject(DomSanitizer);

  // Method to create safe HTML in the component
  createSafeHtml(htmlString: string): SafeHtml {
    return this.sanitizer.bypassSecurityTrustHtml(htmlString);
  }
}
