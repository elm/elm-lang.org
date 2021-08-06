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

  const template = document.createElement('template');
  template.innerHTML = "<div id=\"divider\"></div>";

  class ColumnDivider extends HTMLElement {
      constructor() {
        super();
        this._percentage = 50;
        this._init = this._init.bind(this);
        this._updateDivider = this._updateDivider.bind(this);
      }

      connectedCallback() {
        this._init();
      }

      disconnectedCallback() {
        this._percentage = 50;
      }

      _init() {
        const fragment = template.content.cloneNode(true);
        this.setAttribute('id', "divider");

        const sendDownEvent = debounce(() => {
          this.dispatchEvent(new Event('down'));
        });

        const sendMoveEvent = debounce(() => {
          this.dispatchEvent(new Event('move'));
        });

        const sendUpEvent = debounce(() => {
          this.dispatchEvent(new Event('up'));
        });

        this.addEventListener('mousedown', function(e) {
          sendDownEvent();
          document.body.addEventListener('mouseup', dividerUp);
          document.body.addEventListener('mousemove', dividerMove);
        });

        const dividerUp = (function() {
          sendUpEvent();
          document.body.removeEventListener('mouseup', dividerUp);
          document.body.removeEventListener('mousemove', dividerMove);
        }).bind(this);

        const dividerMove = (function(e) {
          if (e.buttons === 0) {
            dividerUp();
            return;
          }

          var updated = 100 * (e.pageX / window.innerWidth);
          this._percentage = updated >= 98 ? 98 : (updated < 2.5 ? 2.5 : updated);
          this._updateDivider();
          sendMoveEvent();
        }).bind(this);
      }

      _updateDivider() {
        this.style.left = this._percentage + '%';
        if (this._percentage >= 98) {
          this.style.width = '90px';
        } else {
          this.style.width = '10px';
        }
      }

      get percentage() {
        return this._percentage;
      }

      set percentage(updated) {
        this._percentage = updated;
        this._updateDivider();
      }
  }

  customElements.define('column-divider', ColumnDivider);

})();