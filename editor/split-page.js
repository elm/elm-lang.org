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
        var observer = new MutationObserver(function(mutations) {
          this._init();
        });

        this._init();
        observer.observe(this, { childList: true });
      }

      disconnectedCallback() {
        this._split = 50;
      }

      _init() {
        this.style = "width: 100%; display: flex;"
        const [a, b] = this.children;
        a.style.width = this._split + "%";
        b.style.width = (100 - this._split) + "%";
        const fragment = template.content.cloneNode(true);
        this.insertBefore(fragment, b);
        const divider = this.querySelector('#divider');

        const sendMoveEvent = debounce(() => {
          this.dispatchEvent(new Event('move'));
        });

        divider.addEventListener('mousedown', function(e) {
          a.style.pointerEvents = 'none';
          b.style.pointerEvents = 'none';
          document.body.addEventListener('mouseup', dividerUp);
          document.body.addEventListener('mousemove', dividerMove);
        });

        const dividerUp = (function() {
          a.style.pointerEvents = 'auto';
          b.style.pointerEvents = 'auto';
          document.body.removeEventListener('mouseup', dividerUp);
          document.body.removeEventListener('mousemove', dividerMove);
        }).bind(this);

        const dividerMove = (function(e) {
          if (e.buttons === 0) {
            dividerUp();
            return;
          }

          var fraction = 100 * (e.pageX / window.innerWidth);
          divider.style.left = fraction + '%';
          a.style.width = fraction + '%';
          b.style.width = 100 - fraction + '%';
          this._split = fraction;
          sendMoveEvent();
        }).bind(this);
      }

      get split() {
        return this._split;
      }

      set split(updated) {
        this._split = updated;
      }
  }

  customElements.define('split-page', SplitPage);

})();