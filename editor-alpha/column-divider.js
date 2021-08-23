(function() {

  var getTouchEvent = function(e) {
    return e.touches[0] || e.changedTouches[0];
  }

  function ColumnDivider() {
    this._pixels = null;
    this._isClick = true;
    this._init = this._init.bind(this);

    return Reflect.construct(HTMLElement, [], this.constructor);
  }

  ColumnDivider.prototype = Object.create(HTMLElement.prototype, {
    constructor: {
      value: ColumnDivider
    },

    connectedCallback: {
      value: function () {
        this._init();
      }
    },

    disconnectedCallback: {
      value: function () {
        this._pixels = null;
        this._isClick = true;
      }
    },

    _init: {
      value: function () {
        this.setAttribute('id', 'divider');

        var sendDownEvent = (function() {
          this.dispatchEvent(new Event('down'));
        }).bind(this);

        var sendMoveEvent = (function() {
          this.dispatchEvent(new Event('move'));
        }).bind(this);

        var sendUpEvent = (function() {
          this.dispatchEvent(new Event('up'));
        }).bind(this);

        var sendClickEvent = (function() {
          this.dispatchEvent(new Event('_click'));
        }).bind(this);

        var dividerDown = (function(e) {
          e.stopPropagation();
          e.preventDefault();
          if (e.buttons === 2) { // is right click
            return;
          }

          this._pixels = e.pageX || getTouchEvent(e).pageX;
          sendDownEvent();
          this._isClick = true;
          document.body.addEventListener('mouseup', dividerUp);
          document.body.addEventListener('mouseleave', dividerUp);
          document.body.addEventListener('mousemove', dividerMove);
          document.body.addEventListener('touchend', dividerUp);
          document.body.addEventListener('touchcancel', dividerUp);
          document.body.addEventListener('touchmove', dividerMove);
        }).bind(this);

        var dividerUp = (function(e) {
          this._pixels = e.pageX || getTouchEvent(e).pageX;

          if (this._isClick) {
            sendClickEvent();
          } else {
            sendUpEvent();
          }

          document.body.removeEventListener('mouseup', dividerUp);
          document.body.removeEventListener('mouseleave', dividerUp);
          document.body.removeEventListener('mousemove', dividerMove);
          document.body.removeEventListener('touchend', dividerUp);
          document.body.removeEventListener('touchcancel', dividerUp);
          document.body.removeEventListener('touchmove', dividerMove);
        }).bind(this);

        var dividerMove = (function(e) {
          this._isClick = false;

          if (e.buttons === 0) {
            dividerUp(e);
            return;
          }

          this._pixels = e.pageX || getTouchEvent(e).pageX;
          sendMoveEvent();
        }).bind(this);

        this.addEventListener('mousedown', dividerDown);
        this.addEventListener('touchstart', dividerDown, true);
      }
    },

    pixels: {
      get: function () {
        return this._pixels;
      },
      set: function (updated) {
        this._pixels = updated;
      }
    }
  });

  window.customElements.define('column-divider', ColumnDivider);

})();