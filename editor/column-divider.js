(function() {

  const debounce = func => {
    let token;
    return function() {
      const later = () => {
        token = null;
        func.apply(null, arguments);
      };
      cancelIdleCallback(token);
      token = requestIdleCallback(later);
    };
  };

  class ColumnDivider extends HTMLElement {
      constructor() {
        super();
        this._pixels = null;
        this._isClick = true;
        this._init = this._init.bind(this);
      }

      connectedCallback() {
        this._init();
      }

      disconnectedCallback() {
        this._pixels = null;
        this._isClick = true;
      }

      _init() {
        this.setAttribute('id', 'divider');

        const sendDownEvent = debounce(() => {
          this.dispatchEvent(new Event('down'));
        });

        const sendMoveEvent = debounce(() => {
          this.dispatchEvent(new Event('move'));
        });

        const sendUpEvent = debounce(() => {
          this.dispatchEvent(new Event('up'));
        });

        const sendClickEvent = debounce(() => {
          this.dispatchEvent(new Event('_click'));
        });

        this.addEventListener('mousedown', function(e) {
          if (e.buttons === 2) { // is right click
            return;
          }

          sendDownEvent();
          this._isClick = true;
          document.body.addEventListener('mouseup', dividerUp);
          document.body.addEventListener('mouseleave', dividerUp);
          document.body.addEventListener('mousemove', dividerMove);
        });

        const dividerUp = (function(e) {
          this._pixels = e.pageX;

          window.getSelection().empty();
          window.getSelection().removeAllRanges();

          if (this._isClick) {
            sendClickEvent();
          } else {
            sendUpEvent();
          }

          document.body.removeEventListener('mouseup', dividerUp);
          document.body.removeEventListener('mouseleave', dividerUp);
          document.body.removeEventListener('mousemove', dividerMove);
        }).bind(this);

        const dividerMove = (function(e) {
          this._isClick = false;

          if (e.buttons === 0) {
            dividerUp(e);
            return;
          }

          this._pixels = e.pageX;
          sendMoveEvent();
        }).bind(this);
      }

      get pixels() {
        return this._pixels;
      }

      set pixels(updated) {
        this._pixels = updated;
      }
  }

  customElements.define('column-divider', ColumnDivider);

})();