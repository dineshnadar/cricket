import { 
  Injectable, 
  Signal, 
  signal, 
  computed, 
  ElementRef,
  Renderer2,
  RendererFactory2,
  SecurityContext
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

// Tooltip Position Calculation Interface
export interface TooltipPosition {
  top: number;
  left: number;
}

// Tooltip Service
@Injectable({
  providedIn: 'root'
})
export class TooltipService {
  private renderer: Renderer2;

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
    target: null as ElementRef | null,
    tooltipElement: null as HTMLElement | null
  });

  // Public computed signals for reactive access
  public readonly isVisible = computed(() => this.tooltipState().isVisible);
  public readonly content = computed(() => this.tooltipState().content);
  public readonly position = computed(() => this.tooltipState().position);
  public readonly interactive = computed(() => this.tooltipState().interactive);
  public readonly target = computed(() => this.tooltipState().target);

  constructor(
    private domSanitizer: DomSanitizer,
    rendererFactory: RendererFactory2
  ) {
    this.renderer = rendererFactory.createRenderer(null, null);
  }

  // Create tooltip element
  private createTooltipElement(content: string | SafeHtml, position: string, interactive: boolean): HTMLElement {
    const tooltip = this.renderer.createElement('div');
    this.renderer.addClass(tooltip, 'tooltip');
    this.renderer.addClass(tooltip, `tooltip-${position}`);
    
    if (interactive) {
      this.renderer.addClass(tooltip, 'tooltip-interactive');
    }

    const span = this.renderer.createElement('span');
    this.renderer.addClass(span, 'tooltip-content');
    
    if (interactive) {
      this.renderer.addClass(span, 'selectable');
    }

    // Set innerHTML safely
    span.innerHTML = typeof content === 'string' 
      ? content 
      : this.domSanitizer.sanitize(SecurityContext.HTML, content) || '';

    this.renderer.appendChild(tooltip, span);
    return tooltip;
  }

  // Show tooltip
  public show(config: TooltipConfig, target?: ElementRef): void {
    // Remove any existing tooltip
    this.hide();

    const sanitizedContent = typeof config.message === 'string' 
      ? config.message 
      : this.domSanitizer.sanitize(SecurityContext.HTML, config.message) || '';

    const position = config.position ?? DEFAULT_TOOLTIP_CONFIG.position;
    const interactive = config.interactive ?? DEFAULT_TOOLTIP_CONFIG.interactive;

    if (target) {
      // Create tooltip element
      const tooltipElement = this.createTooltipElement(
        sanitizedContent, 
        position, 
        interactive
      );

      // Append tooltip to target
      this.renderer.appendChild(target.nativeElement, tooltipElement);

      // Update state
      this.tooltipState.update(state => ({
        isVisible: true,
        content: sanitizedContent,
        position: position,
        interactive: interactive,
        target: target,
        tooltipElement: tooltipElement
      }));
    }
  }

  // Hide tooltip
  public hide(): void {
    const state = this.tooltipState();
    
    // Remove tooltip element if it exists
    if (state.target && state.tooltipElement) {
      this.renderer.removeChild(state.target.nativeElement, state.tooltipElement);
    }

    // Reset state
    this.tooltipState.update(state => ({
      isVisible: false,
      content: '',
      position: DEFAULT_TOOLTIP_CONFIG.position,
      interactive: DEFAULT_TOOLTIP_CONFIG.interactive,
      target: null,
      tooltipElement: null
    }));
  }
}
import { 
  Injectable, 
  Signal, 
  signal, 
  computed, 
  ElementRef,
  SecurityContext
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

// Tooltip Position Calculation Interface
export interface TooltipPosition {
  top: number;
  left: number;
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
    target: null as ElementRef | null,
    tooltipPosition: { top: 0, left: 0 } as TooltipPosition
  });

  // Public computed signals for reactive access
  public readonly isVisible = computed(() => this.tooltipState().isVisible);
  public readonly content = computed(() => this.tooltipState().content);
  public readonly position = computed(() => this.tooltipState().position);
  public readonly interactive = computed(() => this.tooltipState().interactive);
  public readonly target = computed(() => this.tooltipState().target);
  public readonly tooltipPosition = computed(() => this.tooltipState().tooltipPosition);

  constructor(private domSanitizer: DomSanitizer) {}

  // Calculate tooltip position
  calculatePosition(target: ElementRef, preferredPosition?: string): TooltipPosition {
    const element = target.nativeElement;
    const rect = element.getBoundingClientRect();
    const scrollTop = window.pageYOffset || document.documentElement.scrollTop;
    const scrollLeft = window.pageXOffset || document.documentElement.scrollLeft;

    // Base positions
    const positions = {
      top: {
        top: rect.top + scrollTop - 10,
        left: rect.left + scrollLeft + (rect.width / 2)
      },
      bottom: {
        top: rect.bottom + scrollTop + 10,
        left: rect.left + scrollLeft + (rect.width / 2)
      },
      left: {
        top: rect.top + scrollTop + (rect.height / 2),
        left: rect.left + scrollLeft - 10
      },
      right: {
        top: rect.top + scrollTop + (rect.height / 2),
        left: rect.right + scrollLeft + 10
      }
    };

    // Use preferred position or default to top
    return positions[preferredPosition as keyof typeof positions] || positions.top;
  }

  // Show tooltip with given configuration
  public show(config: TooltipConfig, target?: ElementRef): void {
    const sanitizedContent = typeof config.message === 'string' 
      ? config.message 
      : this.domSanitizer.sanitize(SecurityContext.HTML, config.message) || '';

    const position = config.position ?? DEFAULT_TOOLTIP_CONFIG.position;
    const tooltipPosition = target 
      ? this.calculatePosition(target, position) 
      : { top: 0, left: 0 };

    this.tooltipState.update(state => ({
      isVisible: true,
      content: sanitizedContent,
      position: position,
      interactive: config.interactive ?? DEFAULT_TOOLTIP_CONFIG.interactive,
      target: target ?? state.target,
      tooltipPosition
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
