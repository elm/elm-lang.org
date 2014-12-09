Elm.Native.RedirectHack = {};
Elm.Native.RedirectHack.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.RedirectHack = elm.Native.RedirectHack || {};
    if (elm.Native.RedirectHack.values) {
        return elm.Native.RedirectHack.values;
    }

    function redirect(href) {
        if (href.length > 0) {
            window.location = href;
        }
    }

    return elm.Native.RedirectHack.values = {
        redirect: redirect
    };
};