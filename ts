private parseStringToObject(input: string): TooltipConfig {
  // Remove surrounding quotes and trim
  const cleanInput = input.trim().replace(/^['"]|['"]$/g, '');

  // Default configuration
  const config: TooltipConfig = {
    message: '',
    position: DEFAULT_TOOLTIP_CONFIG.position,
    delay: DEFAULT_TOOLTIP_CONFIG.delay,
    interactive: DEFAULT_TOOLTIP_CONFIG.interactive
  };

  // If input is empty, return default
  if (!cleanInput) return config;

  // Try parsing different formats
  try {
    // Remove outer braces if present
    const trimmedInput = cleanInput.replace(/^{|}$/g, '').trim();

    // Split by comma, but ignore commas inside quotes
    const pairs = trimmedInput.match(/(?:[^,"']|'[^']*'|"[^"]*")+/g) || [];

    pairs.forEach(pair => {
      // Split key and value
      const [rawKey, rawValue] = pair.split(':').map(s => s.trim());
      
      // Clean key and value
      const key = rawKey.replace(/^['"]|['"]$/g, '');
      const value = rawValue.replace(/^['"]|['"]$/g, '');

      // Map to configuration
      switch(key) {
        case 'message':
          config.message = value;
          break;
        case 'position':
          if (['top', 'bottom', 'left', 'right'].includes(value)) {
            config.position = value as 'top' | 'bottom' | 'left' | 'right';
          }
          break;
        case 'delay':
          const parsedDelay = parseInt(value, 10);
          if (!isNaN(parsedDelay)) {
            config.delay = parsedDelay;
          }
          break;
        case 'interactive':
          config.interactive = value.toLowerCase() === 'true';
          break;
      }
    });

    return config;
  } catch (error) {
    // Fallback to using input as message
    config.message = cleanInput;
    return config;
  }
}

// In the directive
@HostListener('mouseenter')
onMouseEnter(): void {
  this.isMouseInside = true;
  this.clearTimers();

  // Parse configuration
  const config = typeof this.tooltipConfig === 'string'
    ? this.parseStringToObject(this.tooltipConfig)
    : this.tooltipConfig;

  // Set hover timer
  this.hoverTimer = window.setTimeout(() => {
    if (this.isMouseInside) {
      this.tooltipService.show(config, this.el);
    }
  }, config.delay);
}
