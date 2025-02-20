@Directive({
  selector: '[appTooltip]',
  standalone: true
})
export class TooltipDirective implements OnDestroy {
  // Tooltip configuration input
  @Input('appTooltip') tooltipConfig: string | TooltipConfig | any = '';

  // Inject dependencies
  private el = inject(ElementRef);
  private tooltipService = inject(TooltipService);

  // Tracking mouse and timer states
  private isMouseInside = false;
  private hoverTimer: number | null = null;
  private hideTimer: number | null = null;
  private isTooltipVisible = false; // New flag to prevent multiple tooltips

  // Mouse enter event handler
  @HostListener('mouseenter')
  onMouseEnter(): void {
    // Prevent re-triggering if tooltip is already visible
    if (this.isTooltipVisible) return;

    this.isMouseInside = true;
    this.clearTimers();

    // Parse configuration
    const config = typeof this.tooltipConfig === 'string'
      ? this.parseStringToObject(this.tooltipConfig)
      : this.tooltipConfig;

    // Set hover timer
    this.hoverTimer = window.setTimeout(() => {
      if (this.isMouseInside && !this.isTooltipVisible) {
        this.tooltipService.show(config, this.el);
        this.isTooltipVisible = true;
      }
    }, config.delay);
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
        this.isTooltipVisible = false;
      }
    }, this.tooltipConfig?.delay ?? DEFAULT_TOOLTIP_CONFIG.delay);
  }

  // Modify service to trigger a callback when tooltip is hidden
  private setupTooltipHideListener() {
    this.tooltipService.onHide(() => {
      this.isTooltipVisible = false;
    });
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


@Injectable({
  providedIn: 'root'
})
export class TooltipService {
  // ... existing code ...

  private hideCallback: (() => void) | null = null;

  // Method to register a hide callback
  onHide(callback: () => void): void {
    this.hideCallback = callback;
  }

  // Modify hide method to trigger callback
  public hide(): void {
    const state = this.tooltipState();
    
    // Remove tooltip element if it exists
    if (state.target && state.tooltipElement) {
      this.renderer.removeChild(state.target.nativeElement, state.tooltipElement);
    }

    // Trigger hide callback if exists
    if (this.hideCallback) {
      this.hideCallback();
      this.hideCallback = null;
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
