@HostListener('mouseenter')
onMouseEnter(): void {
  this.isMouseInside = true;
  this.clearTimers();

  // Handle different input types
  const config = typeof this.tooltipConfig === 'string'
    ? this.parseStringToObject(this.tooltipConfig)
    : {
        // Merge with defaults if it's an object
        message: this.tooltipConfig.message ?? DEFAULT_TOOLTIP_CONFIG.message,
        position: this.tooltipConfig.position ?? DEFAULT_TOOLTIP_CONFIG.position,
        delay: this.tooltipConfig.delay ?? DEFAULT_TOOLTIP_CONFIG.delay,
        interactive: this.tooltipConfig.interactive ?? DEFAULT_TOOLTIP_CONFIG.interactive
      };

  // Set hover timer
  this.hoverTimer = window.setTimeout(() => {
    if (this.isMouseInside) {
      this.tooltipService.show(config, this.el);
    }
  }, config.delay);
}
