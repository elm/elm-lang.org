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

  class SplitPage extends HTMLElement {
      constructor() {
        super();
        this._split = 50;
        this._init = this._init.bind(this);
      }

      connectedCallback() {
        this._init();
      }

      disconnectedCallback() {
        this._split = 50;
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

          var fraction = 100 * (e.pageX / window.innerWidth);
          this.style.left = fraction + '%';
          this._split = fraction;
          sendMoveEvent();
        }).bind(this);
      }

      get split() {
        return this._split;
      }

      set split(updated) {
        this._split = updated;
        this.style.left = updated + '%';
      }
  }

  customElements.define('split-page', SplitPage);

})();