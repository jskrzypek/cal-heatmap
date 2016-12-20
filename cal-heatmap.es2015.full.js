var ascending = function(a, b) {
  return a < b ? -1 : a > b ? 1 : a >= b ? 0 : NaN;
};

var bisector = function(compare) {
  if (compare.length === 1) compare = ascendingComparator(compare);
  return {
    left: function(a, x, lo, hi) {
      if (lo == null) lo = 0;
      if (hi == null) hi = a.length;
      while (lo < hi) {
        var mid = lo + hi >>> 1;
        if (compare(a[mid], x) < 0) lo = mid + 1;
        else hi = mid;
      }
      return lo;
    },
    right: function(a, x, lo, hi) {
      if (lo == null) lo = 0;
      if (hi == null) hi = a.length;
      while (lo < hi) {
        var mid = lo + hi >>> 1;
        if (compare(a[mid], x) > 0) hi = mid;
        else lo = mid + 1;
      }
      return lo;
    }
  };
};

function ascendingComparator(f) {
  return function(d, x) {
    return ascending(f(d), x);
  };
}

var ascendingBisect = bisector(ascending);
var bisectRight = ascendingBisect.right;

var number = function(x) {
  return x === null ? NaN : +x;
};

var extent = function(array, f) {
  var i = -1,
      n = array.length,
      a,
      b,
      c;

  if (f == null) {
    while (++i < n) if ((b = array[i]) != null && b >= b) { a = c = b; break; }
    while (++i < n) if ((b = array[i]) != null) {
      if (a > b) a = b;
      if (c < b) c = b;
    }
  }

  else {
    while (++i < n) if ((b = f(array[i], i, array)) != null && b >= b) { a = c = b; break; }
    while (++i < n) if ((b = f(array[i], i, array)) != null) {
      if (a > b) a = b;
      if (c < b) c = b;
    }
  }

  return [a, c];
};

var array = Array.prototype;

var slice = array.slice;
var map = array.map;

var constant = function(x) {
  return function() {
    return x;
  };
};

var identity = function(x) {
  return x;
};

var sequence = function(start, stop, step) {
  start = +start, stop = +stop, step = (n = arguments.length) < 2 ? (stop = start, start = 0, 1) : n < 3 ? 1 : +step;

  var i = -1,
      n = Math.max(0, Math.ceil((stop - start) / step)) | 0,
      range = new Array(n);

  while (++i < n) {
    range[i] = start + i * step;
  }

  return range;
};

var e10 = Math.sqrt(50);
var e5 = Math.sqrt(10);
var e2 = Math.sqrt(2);

var ticks = function(start, stop, count) {
  var step = tickStep(start, stop, count);
  return sequence(
    Math.ceil(start / step) * step,
    Math.floor(stop / step) * step + step / 2, // inclusive
    step
  );
};

function tickStep(start, stop, count) {
  var step0 = Math.abs(stop - start) / Math.max(0, count),
      step1 = Math.pow(10, Math.floor(Math.log(step0) / Math.LN10)),
      error = step0 / step1;
  if (error >= e10) step1 *= 10;
  else if (error >= e5) step1 *= 5;
  else if (error >= e2) step1 *= 2;
  return stop < start ? -step1 : step1;
}

var sturges = function(values) {
  return Math.ceil(Math.log(values.length) / Math.LN2) + 1;
};

var threshold = function(array, p, f) {
  if (f == null) f = number;
  if (!(n = array.length)) return;
  if ((p = +p) <= 0 || n < 2) return +f(array[0], 0, array);
  if (p >= 1) return +f(array[n - 1], n - 1, array);
  var n,
      h = (n - 1) * p,
      i = Math.floor(h),
      a = +f(array[i], i, array),
      b = +f(array[i + 1], i + 1, array);
  return a + (b - a) * (h - i);
};

var max = function(array, f) {
  var i = -1,
      n = array.length,
      a,
      b;

  if (f == null) {
    while (++i < n) if ((b = array[i]) != null && b >= b) { a = b; break; }
    while (++i < n) if ((b = array[i]) != null && b > a) a = b;
  }

  else {
    while (++i < n) if ((b = f(array[i], i, array)) != null && b >= b) { a = b; break; }
    while (++i < n) if ((b = f(array[i], i, array)) != null && b > a) a = b;
  }

  return a;
};

var min = function(array, f) {
  var i = -1,
      n = array.length,
      a,
      b;

  if (f == null) {
    while (++i < n) if ((b = array[i]) != null && b >= b) { a = b; break; }
    while (++i < n) if ((b = array[i]) != null && a > b) a = b;
  }

  else {
    while (++i < n) if ((b = f(array[i], i, array)) != null && b >= b) { a = b; break; }
    while (++i < n) if ((b = f(array[i], i, array)) != null && a > b) a = b;
  }

  return a;
};

function length(d) {
  return d.length;
}

var prefix = "$";

function Map() {}

Map.prototype = map$1.prototype = {
  constructor: Map,
  has: function(key) {
    return (prefix + key) in this;
  },
  get: function(key) {
    return this[prefix + key];
  },
  set: function(key, value) {
    this[prefix + key] = value;
    return this;
  },
  remove: function(key) {
    var property = prefix + key;
    return property in this && delete this[property];
  },
  clear: function() {
    for (var property in this) if (property[0] === prefix) delete this[property];
  },
  keys: function() {
    var keys = [];
    for (var property in this) if (property[0] === prefix) keys.push(property.slice(1));
    return keys;
  },
  values: function() {
    var values = [];
    for (var property in this) if (property[0] === prefix) values.push(this[property]);
    return values;
  },
  entries: function() {
    var entries = [];
    for (var property in this) if (property[0] === prefix) entries.push({key: property.slice(1), value: this[property]});
    return entries;
  },
  size: function() {
    var size = 0;
    for (var property in this) if (property[0] === prefix) ++size;
    return size;
  },
  empty: function() {
    for (var property in this) if (property[0] === prefix) return false;
    return true;
  },
  each: function(f) {
    for (var property in this) if (property[0] === prefix) f(this[property], property.slice(1), this);
  }
};

function map$1(object, f) {
  var map = new Map;

  // Copy constructor.
  if (object instanceof Map) object.each(function(value, key) { map.set(key, value); });

  // Index array by numeric index or specified key function.
  else if (Array.isArray(object)) {
    var i = -1,
        n = object.length,
        o;

    if (f == null) while (++i < n) map.set(i, object[i]);
    else while (++i < n) map.set(f(o = object[i], i, object), o);
  }

  // Convert object to map.
  else if (object) for (var key in object) map.set(key, object[key]);

  return map;
}

function createObject() {
  return {};
}

function setObject(object, key, value) {
  object[key] = value;
}

function createMap() {
  return map$1();
}

function setMap(map, key, value) {
  map.set(key, value);
}

function Set() {}

var proto = map$1.prototype;

Set.prototype = set.prototype = {
  constructor: Set,
  has: proto.has,
  add: function(value) {
    value += "";
    this[prefix + value] = value;
    return this;
  },
  remove: proto.remove,
  clear: proto.clear,
  values: proto.keys,
  size: proto.size,
  empty: proto.empty,
  each: proto.each
};

function set(object, f) {
  var set = new Set;

  // Copy constructor.
  if (object instanceof Set) object.each(function(value) { set.add(value); });

  // Otherwise, assume it’s an array.
  else if (object) {
    var i = -1, n = object.length;
    if (f == null) while (++i < n) set.add(object[i]);
    else while (++i < n) set.add(f(object[i], i, object));
  }

  return set;
}

// Computes the decimal coefficient and exponent of the specified number x with
// significant digits p, where x is positive and p is in [1, 21] or undefined.
// For example, formatDecimal(1.23) returns ["123", 0].
var formatDecimal = function(x, p) {
  if ((i = (x = p ? x.toExponential(p - 1) : x.toExponential()).indexOf("e")) < 0) return null; // NaN, ±Infinity
  var i, coefficient = x.slice(0, i);

  // The string returned by toExponential either has the form \d\.\d+e[-+]\d+
  // (e.g., 1.2e+3) or the form \de[-+]\d+ (e.g., 1e+3).
  return [
    coefficient.length > 1 ? coefficient[0] + coefficient.slice(2) : coefficient,
    +x.slice(i + 1)
  ];
};

var exponent = function(x) {
  return x = formatDecimal(Math.abs(x)), x ? x[1] : NaN;
};

var formatGroup = function(grouping, thousands) {
  return function(value, width) {
    var i = value.length,
        t = [],
        j = 0,
        g = grouping[0],
        length = 0;

    while (i > 0 && g > 0) {
      if (length + g + 1 > width) g = Math.max(1, width - length);
      t.push(value.substring(i -= g, i + g));
      if ((length += g + 1) > width) break;
      g = grouping[j = (j + 1) % grouping.length];
    }

    return t.reverse().join(thousands);
  };
};

var formatDefault = function(x, p) {
  x = x.toPrecision(p);

  out: for (var n = x.length, i = 1, i0 = -1, i1; i < n; ++i) {
    switch (x[i]) {
      case ".": i0 = i1 = i; break;
      case "0": if (i0 === 0) i0 = i; i1 = i; break;
      case "e": break out;
      default: if (i0 > 0) i0 = 0; break;
    }
  }

  return i0 > 0 ? x.slice(0, i0) + x.slice(i1 + 1) : x;
};

var prefixExponent;

var formatPrefixAuto = function(x, p) {
  var d = formatDecimal(x, p);
  if (!d) return x + "";
  var coefficient = d[0],
      exponent = d[1],
      i = exponent - (prefixExponent = Math.max(-8, Math.min(8, Math.floor(exponent / 3))) * 3) + 1,
      n = coefficient.length;
  return i === n ? coefficient
      : i > n ? coefficient + new Array(i - n + 1).join("0")
      : i > 0 ? coefficient.slice(0, i) + "." + coefficient.slice(i)
      : "0." + new Array(1 - i).join("0") + formatDecimal(x, Math.max(0, p + i - 1))[0]; // less than 1y!
};

var formatRounded = function(x, p) {
  var d = formatDecimal(x, p);
  if (!d) return x + "";
  var coefficient = d[0],
      exponent = d[1];
  return exponent < 0 ? "0." + new Array(-exponent).join("0") + coefficient
      : coefficient.length > exponent + 1 ? coefficient.slice(0, exponent + 1) + "." + coefficient.slice(exponent + 1)
      : coefficient + new Array(exponent - coefficient.length + 2).join("0");
};

var formatTypes = {
  "": formatDefault,
  "%": function(x, p) { return (x * 100).toFixed(p); },
  "b": function(x) { return Math.round(x).toString(2); },
  "c": function(x) { return x + ""; },
  "d": function(x) { return Math.round(x).toString(10); },
  "e": function(x, p) { return x.toExponential(p); },
  "f": function(x, p) { return x.toFixed(p); },
  "g": function(x, p) { return x.toPrecision(p); },
  "o": function(x) { return Math.round(x).toString(8); },
  "p": function(x, p) { return formatRounded(x * 100, p); },
  "r": formatRounded,
  "s": formatPrefixAuto,
  "X": function(x) { return Math.round(x).toString(16).toUpperCase(); },
  "x": function(x) { return Math.round(x).toString(16); }
};

// [[fill]align][sign][symbol][0][width][,][.precision][type]
var re = /^(?:(.)?([<>=^]))?([+\-\( ])?([$#])?(0)?(\d+)?(,)?(\.\d+)?([a-z%])?$/i;

var formatSpecifier = function(specifier) {
  return new FormatSpecifier(specifier);
};

function FormatSpecifier(specifier) {
  if (!(match = re.exec(specifier))) throw new Error("invalid format: " + specifier);

  var match,
      fill = match[1] || " ",
      align = match[2] || ">",
      sign = match[3] || "-",
      symbol = match[4] || "",
      zero = !!match[5],
      width = match[6] && +match[6],
      comma = !!match[7],
      precision = match[8] && +match[8].slice(1),
      type = match[9] || "";

  // The "n" type is an alias for ",g".
  if (type === "n") comma = true, type = "g";

  // Map invalid types to the default format.
  else if (!formatTypes[type]) type = "";

  // If zero fill is specified, padding goes after sign and before digits.
  if (zero || (fill === "0" && align === "=")) zero = true, fill = "0", align = "=";

  this.fill = fill;
  this.align = align;
  this.sign = sign;
  this.symbol = symbol;
  this.zero = zero;
  this.width = width;
  this.comma = comma;
  this.precision = precision;
  this.type = type;
}

FormatSpecifier.prototype.toString = function() {
  return this.fill
      + this.align
      + this.sign
      + this.symbol
      + (this.zero ? "0" : "")
      + (this.width == null ? "" : Math.max(1, this.width | 0))
      + (this.comma ? "," : "")
      + (this.precision == null ? "" : "." + Math.max(0, this.precision | 0))
      + this.type;
};

var prefixes = ["y","z","a","f","p","n","µ","m","","k","M","G","T","P","E","Z","Y"];

function identity$1(x) {
  return x;
}

var formatLocale = function(locale) {
  var group = locale.grouping && locale.thousands ? formatGroup(locale.grouping, locale.thousands) : identity$1,
      currency = locale.currency,
      decimal = locale.decimal;

  function newFormat(specifier) {
    specifier = formatSpecifier(specifier);

    var fill = specifier.fill,
        align = specifier.align,
        sign = specifier.sign,
        symbol = specifier.symbol,
        zero = specifier.zero,
        width = specifier.width,
        comma = specifier.comma,
        precision = specifier.precision,
        type = specifier.type;

    // Compute the prefix and suffix.
    // For SI-prefix, the suffix is lazily computed.
    var prefix = symbol === "$" ? currency[0] : symbol === "#" && /[boxX]/.test(type) ? "0" + type.toLowerCase() : "",
        suffix = symbol === "$" ? currency[1] : /[%p]/.test(type) ? "%" : "";

    // What format function should we use?
    // Is this an integer type?
    // Can this type generate exponential notation?
    var formatType = formatTypes[type],
        maybeSuffix = !type || /[defgprs%]/.test(type);

    // Set the default precision if not specified,
    // or clamp the specified precision to the supported range.
    // For significant precision, it must be in [1, 21].
    // For fixed precision, it must be in [0, 20].
    precision = precision == null ? (type ? 6 : 12)
        : /[gprs]/.test(type) ? Math.max(1, Math.min(21, precision))
        : Math.max(0, Math.min(20, precision));

    function format(value) {
      var valuePrefix = prefix,
          valueSuffix = suffix,
          i, n, c;

      if (type === "c") {
        valueSuffix = formatType(value) + valueSuffix;
        value = "";
      } else {
        value = +value;

        // Convert negative to positive, and compute the prefix.
        // Note that -0 is not less than 0, but 1 / -0 is!
        var valueNegative = (value < 0 || 1 / value < 0) && (value *= -1, true);

        // Perform the initial formatting.
        value = formatType(value, precision);

        // If the original value was negative, it may be rounded to zero during
        // formatting; treat this as (positive) zero.
        if (valueNegative) {
          i = -1, n = value.length;
          valueNegative = false;
          while (++i < n) {
            if (c = value.charCodeAt(i), (48 < c && c < 58)
                || (type === "x" && 96 < c && c < 103)
                || (type === "X" && 64 < c && c < 71)) {
              valueNegative = true;
              break;
            }
          }
        }

        // Compute the prefix and suffix.
        valuePrefix = (valueNegative ? (sign === "(" ? sign : "-") : sign === "-" || sign === "(" ? "" : sign) + valuePrefix;
        valueSuffix = valueSuffix + (type === "s" ? prefixes[8 + prefixExponent / 3] : "") + (valueNegative && sign === "(" ? ")" : "");

        // Break the formatted value into the integer “value” part that can be
        // grouped, and fractional or exponential “suffix” part that is not.
        if (maybeSuffix) {
          i = -1, n = value.length;
          while (++i < n) {
            if (c = value.charCodeAt(i), 48 > c || c > 57) {
              valueSuffix = (c === 46 ? decimal + value.slice(i + 1) : value.slice(i)) + valueSuffix;
              value = value.slice(0, i);
              break;
            }
          }
        }
      }

      // If the fill character is not "0", grouping is applied before padding.
      if (comma && !zero) value = group(value, Infinity);

      // Compute the padding.
      var length = valuePrefix.length + value.length + valueSuffix.length,
          padding = length < width ? new Array(width - length + 1).join(fill) : "";

      // If the fill character is "0", grouping is applied after padding.
      if (comma && zero) value = group(padding + value, padding.length ? width - valueSuffix.length : Infinity), padding = "";

      // Reconstruct the final output based on the desired alignment.
      switch (align) {
        case "<": return valuePrefix + value + valueSuffix + padding;
        case "=": return valuePrefix + padding + value + valueSuffix;
        case "^": return padding.slice(0, length = padding.length >> 1) + valuePrefix + value + valueSuffix + padding.slice(length);
      }
      return padding + valuePrefix + value + valueSuffix;
    }

    format.toString = function() {
      return specifier + "";
    };

    return format;
  }

  function formatPrefix(specifier, value) {
    var f = newFormat((specifier = formatSpecifier(specifier), specifier.type = "f", specifier)),
        e = Math.max(-8, Math.min(8, Math.floor(exponent(value) / 3))) * 3,
        k = Math.pow(10, -e),
        prefix = prefixes[8 + e / 3];
    return function(value) {
      return f(k * value) + prefix;
    };
  }

  return {
    format: newFormat,
    formatPrefix: formatPrefix
  };
};

var locale$1;
var format;
var formatPrefix;

defaultLocale({
  decimal: ".",
  thousands: ",",
  grouping: [3],
  currency: ["$", ""]
});

function defaultLocale(definition) {
  locale$1 = formatLocale(definition);
  format = locale$1.format;
  formatPrefix = locale$1.formatPrefix;
  return locale$1;
}

var precisionFixed = function(step) {
  return Math.max(0, -exponent(Math.abs(step)));
};

var precisionPrefix = function(step, value) {
  return Math.max(0, Math.max(-8, Math.min(8, Math.floor(exponent(value) / 3))) * 3 - exponent(Math.abs(step)));
};

var precisionRound = function(step, max) {
  step = Math.abs(step), max = Math.abs(max) - step;
  return Math.max(0, exponent(max) - exponent(step)) + 1;
};

var define = function(constructor, factory, prototype) {
  constructor.prototype = factory.prototype = prototype;
  prototype.constructor = constructor;
};

function extend(parent, definition) {
  var prototype = Object.create(parent.prototype);
  for (var key in definition) prototype[key] = definition[key];
  return prototype;
}

function Color() {}

var darker = 0.7;
var brighter = 1 / darker;

var reI = "\\s*([+-]?\\d+)\\s*";
var reN = "\\s*([+-]?\\d*\\.?\\d+(?:[eE][+-]?\\d+)?)\\s*";
var reP = "\\s*([+-]?\\d*\\.?\\d+(?:[eE][+-]?\\d+)?)%\\s*";
var reHex3 = /^#([0-9a-f]{3})$/;
var reHex6 = /^#([0-9a-f]{6})$/;
var reRgbInteger = new RegExp("^rgb\\(" + [reI, reI, reI] + "\\)$");
var reRgbPercent = new RegExp("^rgb\\(" + [reP, reP, reP] + "\\)$");
var reRgbaInteger = new RegExp("^rgba\\(" + [reI, reI, reI, reN] + "\\)$");
var reRgbaPercent = new RegExp("^rgba\\(" + [reP, reP, reP, reN] + "\\)$");
var reHslPercent = new RegExp("^hsl\\(" + [reN, reP, reP] + "\\)$");
var reHslaPercent = new RegExp("^hsla\\(" + [reN, reP, reP, reN] + "\\)$");

var named = {
  aliceblue: 0xf0f8ff,
  antiquewhite: 0xfaebd7,
  aqua: 0x00ffff,
  aquamarine: 0x7fffd4,
  azure: 0xf0ffff,
  beige: 0xf5f5dc,
  bisque: 0xffe4c4,
  black: 0x000000,
  blanchedalmond: 0xffebcd,
  blue: 0x0000ff,
  blueviolet: 0x8a2be2,
  brown: 0xa52a2a,
  burlywood: 0xdeb887,
  cadetblue: 0x5f9ea0,
  chartreuse: 0x7fff00,
  chocolate: 0xd2691e,
  coral: 0xff7f50,
  cornflowerblue: 0x6495ed,
  cornsilk: 0xfff8dc,
  crimson: 0xdc143c,
  cyan: 0x00ffff,
  darkblue: 0x00008b,
  darkcyan: 0x008b8b,
  darkgoldenrod: 0xb8860b,
  darkgray: 0xa9a9a9,
  darkgreen: 0x006400,
  darkgrey: 0xa9a9a9,
  darkkhaki: 0xbdb76b,
  darkmagenta: 0x8b008b,
  darkolivegreen: 0x556b2f,
  darkorange: 0xff8c00,
  darkorchid: 0x9932cc,
  darkred: 0x8b0000,
  darksalmon: 0xe9967a,
  darkseagreen: 0x8fbc8f,
  darkslateblue: 0x483d8b,
  darkslategray: 0x2f4f4f,
  darkslategrey: 0x2f4f4f,
  darkturquoise: 0x00ced1,
  darkviolet: 0x9400d3,
  deeppink: 0xff1493,
  deepskyblue: 0x00bfff,
  dimgray: 0x696969,
  dimgrey: 0x696969,
  dodgerblue: 0x1e90ff,
  firebrick: 0xb22222,
  floralwhite: 0xfffaf0,
  forestgreen: 0x228b22,
  fuchsia: 0xff00ff,
  gainsboro: 0xdcdcdc,
  ghostwhite: 0xf8f8ff,
  gold: 0xffd700,
  goldenrod: 0xdaa520,
  gray: 0x808080,
  green: 0x008000,
  greenyellow: 0xadff2f,
  grey: 0x808080,
  honeydew: 0xf0fff0,
  hotpink: 0xff69b4,
  indianred: 0xcd5c5c,
  indigo: 0x4b0082,
  ivory: 0xfffff0,
  khaki: 0xf0e68c,
  lavender: 0xe6e6fa,
  lavenderblush: 0xfff0f5,
  lawngreen: 0x7cfc00,
  lemonchiffon: 0xfffacd,
  lightblue: 0xadd8e6,
  lightcoral: 0xf08080,
  lightcyan: 0xe0ffff,
  lightgoldenrodyellow: 0xfafad2,
  lightgray: 0xd3d3d3,
  lightgreen: 0x90ee90,
  lightgrey: 0xd3d3d3,
  lightpink: 0xffb6c1,
  lightsalmon: 0xffa07a,
  lightseagreen: 0x20b2aa,
  lightskyblue: 0x87cefa,
  lightslategray: 0x778899,
  lightslategrey: 0x778899,
  lightsteelblue: 0xb0c4de,
  lightyellow: 0xffffe0,
  lime: 0x00ff00,
  limegreen: 0x32cd32,
  linen: 0xfaf0e6,
  magenta: 0xff00ff,
  maroon: 0x800000,
  mediumaquamarine: 0x66cdaa,
  mediumblue: 0x0000cd,
  mediumorchid: 0xba55d3,
  mediumpurple: 0x9370db,
  mediumseagreen: 0x3cb371,
  mediumslateblue: 0x7b68ee,
  mediumspringgreen: 0x00fa9a,
  mediumturquoise: 0x48d1cc,
  mediumvioletred: 0xc71585,
  midnightblue: 0x191970,
  mintcream: 0xf5fffa,
  mistyrose: 0xffe4e1,
  moccasin: 0xffe4b5,
  navajowhite: 0xffdead,
  navy: 0x000080,
  oldlace: 0xfdf5e6,
  olive: 0x808000,
  olivedrab: 0x6b8e23,
  orange: 0xffa500,
  orangered: 0xff4500,
  orchid: 0xda70d6,
  palegoldenrod: 0xeee8aa,
  palegreen: 0x98fb98,
  paleturquoise: 0xafeeee,
  palevioletred: 0xdb7093,
  papayawhip: 0xffefd5,
  peachpuff: 0xffdab9,
  peru: 0xcd853f,
  pink: 0xffc0cb,
  plum: 0xdda0dd,
  powderblue: 0xb0e0e6,
  purple: 0x800080,
  rebeccapurple: 0x663399,
  red: 0xff0000,
  rosybrown: 0xbc8f8f,
  royalblue: 0x4169e1,
  saddlebrown: 0x8b4513,
  salmon: 0xfa8072,
  sandybrown: 0xf4a460,
  seagreen: 0x2e8b57,
  seashell: 0xfff5ee,
  sienna: 0xa0522d,
  silver: 0xc0c0c0,
  skyblue: 0x87ceeb,
  slateblue: 0x6a5acd,
  slategray: 0x708090,
  slategrey: 0x708090,
  snow: 0xfffafa,
  springgreen: 0x00ff7f,
  steelblue: 0x4682b4,
  tan: 0xd2b48c,
  teal: 0x008080,
  thistle: 0xd8bfd8,
  tomato: 0xff6347,
  turquoise: 0x40e0d0,
  violet: 0xee82ee,
  wheat: 0xf5deb3,
  white: 0xffffff,
  whitesmoke: 0xf5f5f5,
  yellow: 0xffff00,
  yellowgreen: 0x9acd32
};

define(Color, color, {
  displayable: function() {
    return this.rgb().displayable();
  },
  toString: function() {
    return this.rgb() + "";
  }
});

function color(format) {
  var m;
  format = (format + "").trim().toLowerCase();
  return (m = reHex3.exec(format)) ? (m = parseInt(m[1], 16), new Rgb((m >> 8 & 0xf) | (m >> 4 & 0x0f0), (m >> 4 & 0xf) | (m & 0xf0), ((m & 0xf) << 4) | (m & 0xf), 1)) // #f00
      : (m = reHex6.exec(format)) ? rgbn(parseInt(m[1], 16)) // #ff0000
      : (m = reRgbInteger.exec(format)) ? new Rgb(m[1], m[2], m[3], 1) // rgb(255, 0, 0)
      : (m = reRgbPercent.exec(format)) ? new Rgb(m[1] * 255 / 100, m[2] * 255 / 100, m[3] * 255 / 100, 1) // rgb(100%, 0%, 0%)
      : (m = reRgbaInteger.exec(format)) ? rgba(m[1], m[2], m[3], m[4]) // rgba(255, 0, 0, 1)
      : (m = reRgbaPercent.exec(format)) ? rgba(m[1] * 255 / 100, m[2] * 255 / 100, m[3] * 255 / 100, m[4]) // rgb(100%, 0%, 0%, 1)
      : (m = reHslPercent.exec(format)) ? hsla(m[1], m[2] / 100, m[3] / 100, 1) // hsl(120, 50%, 50%)
      : (m = reHslaPercent.exec(format)) ? hsla(m[1], m[2] / 100, m[3] / 100, m[4]) // hsla(120, 50%, 50%, 1)
      : named.hasOwnProperty(format) ? rgbn(named[format])
      : format === "transparent" ? new Rgb(NaN, NaN, NaN, 0)
      : null;
}

function rgbn(n) {
  return new Rgb(n >> 16 & 0xff, n >> 8 & 0xff, n & 0xff, 1);
}

function rgba(r, g, b, a) {
  if (a <= 0) r = g = b = NaN;
  return new Rgb(r, g, b, a);
}

function rgbConvert(o) {
  if (!(o instanceof Color)) o = color(o);
  if (!o) return new Rgb;
  o = o.rgb();
  return new Rgb(o.r, o.g, o.b, o.opacity);
}

function rgb(r, g, b, opacity) {
  return arguments.length === 1 ? rgbConvert(r) : new Rgb(r, g, b, opacity == null ? 1 : opacity);
}

function Rgb(r, g, b, opacity) {
  this.r = +r;
  this.g = +g;
  this.b = +b;
  this.opacity = +opacity;
}

define(Rgb, rgb, extend(Color, {
  brighter: function(k) {
    k = k == null ? brighter : Math.pow(brighter, k);
    return new Rgb(this.r * k, this.g * k, this.b * k, this.opacity);
  },
  darker: function(k) {
    k = k == null ? darker : Math.pow(darker, k);
    return new Rgb(this.r * k, this.g * k, this.b * k, this.opacity);
  },
  rgb: function() {
    return this;
  },
  displayable: function() {
    return (0 <= this.r && this.r <= 255)
        && (0 <= this.g && this.g <= 255)
        && (0 <= this.b && this.b <= 255)
        && (0 <= this.opacity && this.opacity <= 1);
  },
  toString: function() {
    var a = this.opacity; a = isNaN(a) ? 1 : Math.max(0, Math.min(1, a));
    return (a === 1 ? "rgb(" : "rgba(")
        + Math.max(0, Math.min(255, Math.round(this.r) || 0)) + ", "
        + Math.max(0, Math.min(255, Math.round(this.g) || 0)) + ", "
        + Math.max(0, Math.min(255, Math.round(this.b) || 0))
        + (a === 1 ? ")" : ", " + a + ")");
  }
}));

function hsla(h, s, l, a) {
  if (a <= 0) h = s = l = NaN;
  else if (l <= 0 || l >= 1) h = s = NaN;
  else if (s <= 0) h = NaN;
  return new Hsl(h, s, l, a);
}

function hslConvert(o) {
  if (o instanceof Hsl) return new Hsl(o.h, o.s, o.l, o.opacity);
  if (!(o instanceof Color)) o = color(o);
  if (!o) return new Hsl;
  if (o instanceof Hsl) return o;
  o = o.rgb();
  var r = o.r / 255,
      g = o.g / 255,
      b = o.b / 255,
      min = Math.min(r, g, b),
      max = Math.max(r, g, b),
      h = NaN,
      s = max - min,
      l = (max + min) / 2;
  if (s) {
    if (r === max) h = (g - b) / s + (g < b) * 6;
    else if (g === max) h = (b - r) / s + 2;
    else h = (r - g) / s + 4;
    s /= l < 0.5 ? max + min : 2 - max - min;
    h *= 60;
  } else {
    s = l > 0 && l < 1 ? 0 : h;
  }
  return new Hsl(h, s, l, o.opacity);
}

function hsl(h, s, l, opacity) {
  return arguments.length === 1 ? hslConvert(h) : new Hsl(h, s, l, opacity == null ? 1 : opacity);
}

function Hsl(h, s, l, opacity) {
  this.h = +h;
  this.s = +s;
  this.l = +l;
  this.opacity = +opacity;
}

define(Hsl, hsl, extend(Color, {
  brighter: function(k) {
    k = k == null ? brighter : Math.pow(brighter, k);
    return new Hsl(this.h, this.s, this.l * k, this.opacity);
  },
  darker: function(k) {
    k = k == null ? darker : Math.pow(darker, k);
    return new Hsl(this.h, this.s, this.l * k, this.opacity);
  },
  rgb: function() {
    var h = this.h % 360 + (this.h < 0) * 360,
        s = isNaN(h) || isNaN(this.s) ? 0 : this.s,
        l = this.l,
        m2 = l + (l < 0.5 ? l : 1 - l) * s,
        m1 = 2 * l - m2;
    return new Rgb(
      hsl2rgb(h >= 240 ? h - 240 : h + 120, m1, m2),
      hsl2rgb(h, m1, m2),
      hsl2rgb(h < 120 ? h + 240 : h - 120, m1, m2),
      this.opacity
    );
  },
  displayable: function() {
    return (0 <= this.s && this.s <= 1 || isNaN(this.s))
        && (0 <= this.l && this.l <= 1)
        && (0 <= this.opacity && this.opacity <= 1);
  }
}));

/* From FvD 13.37, CSS Color Module Level 3 */
function hsl2rgb(h, m1, m2) {
  return (h < 60 ? m1 + (m2 - m1) * h / 60
      : h < 180 ? m2
      : h < 240 ? m1 + (m2 - m1) * (240 - h) / 60
      : m1) * 255;
}

var deg2rad = Math.PI / 180;
var rad2deg = 180 / Math.PI;

var Kn = 18;
var Xn = 0.950470;
var Yn = 1;
var Zn = 1.088830;
var t0 = 4 / 29;
var t1 = 6 / 29;
var t2 = 3 * t1 * t1;
var t3 = t1 * t1 * t1;

function labConvert(o) {
  if (o instanceof Lab) return new Lab(o.l, o.a, o.b, o.opacity);
  if (o instanceof Hcl) {
    var h = o.h * deg2rad;
    return new Lab(o.l, Math.cos(h) * o.c, Math.sin(h) * o.c, o.opacity);
  }
  if (!(o instanceof Rgb)) o = rgbConvert(o);
  var b = rgb2xyz(o.r),
      a = rgb2xyz(o.g),
      l = rgb2xyz(o.b),
      x = xyz2lab((0.4124564 * b + 0.3575761 * a + 0.1804375 * l) / Xn),
      y = xyz2lab((0.2126729 * b + 0.7151522 * a + 0.0721750 * l) / Yn),
      z = xyz2lab((0.0193339 * b + 0.1191920 * a + 0.9503041 * l) / Zn);
  return new Lab(116 * y - 16, 500 * (x - y), 200 * (y - z), o.opacity);
}

function lab(l, a, b, opacity) {
  return arguments.length === 1 ? labConvert(l) : new Lab(l, a, b, opacity == null ? 1 : opacity);
}

function Lab(l, a, b, opacity) {
  this.l = +l;
  this.a = +a;
  this.b = +b;
  this.opacity = +opacity;
}

define(Lab, lab, extend(Color, {
  brighter: function(k) {
    return new Lab(this.l + Kn * (k == null ? 1 : k), this.a, this.b, this.opacity);
  },
  darker: function(k) {
    return new Lab(this.l - Kn * (k == null ? 1 : k), this.a, this.b, this.opacity);
  },
  rgb: function() {
    var y = (this.l + 16) / 116,
        x = isNaN(this.a) ? y : y + this.a / 500,
        z = isNaN(this.b) ? y : y - this.b / 200;
    y = Yn * lab2xyz(y);
    x = Xn * lab2xyz(x);
    z = Zn * lab2xyz(z);
    return new Rgb(
      xyz2rgb( 3.2404542 * x - 1.5371385 * y - 0.4985314 * z), // D65 -> sRGB
      xyz2rgb(-0.9692660 * x + 1.8760108 * y + 0.0415560 * z),
      xyz2rgb( 0.0556434 * x - 0.2040259 * y + 1.0572252 * z),
      this.opacity
    );
  }
}));

function xyz2lab(t) {
  return t > t3 ? Math.pow(t, 1 / 3) : t / t2 + t0;
}

function lab2xyz(t) {
  return t > t1 ? t * t * t : t2 * (t - t0);
}

function xyz2rgb(x) {
  return 255 * (x <= 0.0031308 ? 12.92 * x : 1.055 * Math.pow(x, 1 / 2.4) - 0.055);
}

function rgb2xyz(x) {
  return (x /= 255) <= 0.04045 ? x / 12.92 : Math.pow((x + 0.055) / 1.055, 2.4);
}

function hclConvert(o) {
  if (o instanceof Hcl) return new Hcl(o.h, o.c, o.l, o.opacity);
  if (!(o instanceof Lab)) o = labConvert(o);
  var h = Math.atan2(o.b, o.a) * rad2deg;
  return new Hcl(h < 0 ? h + 360 : h, Math.sqrt(o.a * o.a + o.b * o.b), o.l, o.opacity);
}

function hcl(h, c, l, opacity) {
  return arguments.length === 1 ? hclConvert(h) : new Hcl(h, c, l, opacity == null ? 1 : opacity);
}

function Hcl(h, c, l, opacity) {
  this.h = +h;
  this.c = +c;
  this.l = +l;
  this.opacity = +opacity;
}

define(Hcl, hcl, extend(Color, {
  brighter: function(k) {
    return new Hcl(this.h, this.c, this.l + Kn * (k == null ? 1 : k), this.opacity);
  },
  darker: function(k) {
    return new Hcl(this.h, this.c, this.l - Kn * (k == null ? 1 : k), this.opacity);
  },
  rgb: function() {
    return labConvert(this).rgb();
  }
}));

var A = -0.14861;
var B = +1.78277;
var C = -0.29227;
var D = -0.90649;
var E = +1.97294;
var ED = E * D;
var EB = E * B;
var BC_DA = B * C - D * A;

function cubehelixConvert(o) {
  if (o instanceof Cubehelix) return new Cubehelix(o.h, o.s, o.l, o.opacity);
  if (!(o instanceof Rgb)) o = rgbConvert(o);
  var r = o.r / 255,
      g = o.g / 255,
      b = o.b / 255,
      l = (BC_DA * b + ED * r - EB * g) / (BC_DA + ED - EB),
      bl = b - l,
      k = (E * (g - l) - C * bl) / D,
      s = Math.sqrt(k * k + bl * bl) / (E * l * (1 - l)), // NaN if l=0 or l=1
      h = s ? Math.atan2(k, bl) * rad2deg - 120 : NaN;
  return new Cubehelix(h < 0 ? h + 360 : h, s, l, o.opacity);
}

function cubehelix(h, s, l, opacity) {
  return arguments.length === 1 ? cubehelixConvert(h) : new Cubehelix(h, s, l, opacity == null ? 1 : opacity);
}

function Cubehelix(h, s, l, opacity) {
  this.h = +h;
  this.s = +s;
  this.l = +l;
  this.opacity = +opacity;
}

define(Cubehelix, cubehelix, extend(Color, {
  brighter: function(k) {
    k = k == null ? brighter : Math.pow(brighter, k);
    return new Cubehelix(this.h, this.s, this.l * k, this.opacity);
  },
  darker: function(k) {
    k = k == null ? darker : Math.pow(darker, k);
    return new Cubehelix(this.h, this.s, this.l * k, this.opacity);
  },
  rgb: function() {
    var h = isNaN(this.h) ? 0 : (this.h + 120) * deg2rad,
        l = +this.l,
        a = isNaN(this.s) ? 0 : this.s * l * (1 - l),
        cosh = Math.cos(h),
        sinh = Math.sin(h);
    return new Rgb(
      255 * (l + a * (A * cosh + B * sinh)),
      255 * (l + a * (C * cosh + D * sinh)),
      255 * (l + a * (E * cosh)),
      this.opacity
    );
  }
}));

function basis(t1, v0, v1, v2, v3) {
  var t2 = t1 * t1, t3 = t2 * t1;
  return ((1 - 3 * t1 + 3 * t2 - t3) * v0
      + (4 - 6 * t2 + 3 * t3) * v1
      + (1 + 3 * t1 + 3 * t2 - 3 * t3) * v2
      + t3 * v3) / 6;
}

var constant$1 = function(x) {
  return function() {
    return x;
  };
};

function linear(a, d) {
  return function(t) {
    return a + t * d;
  };
}

function exponential(a, b, y) {
  return a = Math.pow(a, y), b = Math.pow(b, y) - a, y = 1 / y, function(t) {
    return Math.pow(a + t * b, y);
  };
}

function hue(a, b) {
  var d = b - a;
  return d ? linear(a, d > 180 || d < -180 ? d - 360 * Math.round(d / 360) : d) : constant$1(isNaN(a) ? b : a);
}

function gamma(y) {
  return (y = +y) === 1 ? nogamma : function(a, b) {
    return b - a ? exponential(a, b, y) : constant$1(isNaN(a) ? b : a);
  };
}

function nogamma(a, b) {
  var d = b - a;
  return d ? linear(a, d) : constant$1(isNaN(a) ? b : a);
}

var rgb$1 = (function rgbGamma(y) {
  var color$$1 = gamma(y);

  function rgb$$1(start, end) {
    var r = color$$1((start = rgb(start)).r, (end = rgb(end)).r),
        g = color$$1(start.g, end.g),
        b = color$$1(start.b, end.b),
        opacity = color$$1(start.opacity, end.opacity);
    return function(t) {
      start.r = r(t);
      start.g = g(t);
      start.b = b(t);
      start.opacity = opacity(t);
      return start + "";
    };
  }

  rgb$$1.gamma = rgbGamma;

  return rgb$$1;
})(1);

var array$1 = function(a, b) {
  var nb = b ? b.length : 0,
      na = a ? Math.min(nb, a.length) : 0,
      x = new Array(nb),
      c = new Array(nb),
      i;

  for (i = 0; i < na; ++i) x[i] = interpolateValue(a[i], b[i]);
  for (; i < nb; ++i) c[i] = b[i];

  return function(t) {
    for (i = 0; i < na; ++i) c[i] = x[i](t);
    return c;
  };
};

var date = function(a, b) {
  var d = new Date;
  return a = +a, b -= a, function(t) {
    return d.setTime(a + b * t), d;
  };
};

var reinterpolate = function(a, b) {
  return a = +a, b -= a, function(t) {
    return a + b * t;
  };
};

var object = function(a, b) {
  var i = {},
      c = {},
      k;

  if (a === null || typeof a !== "object") a = {};
  if (b === null || typeof b !== "object") b = {};

  for (k in b) {
    if (k in a) {
      i[k] = interpolateValue(a[k], b[k]);
    } else {
      c[k] = b[k];
    }
  }

  return function(t) {
    for (k in i) c[k] = i[k](t);
    return c;
  };
};

var reA = /[-+]?(?:\d+\.?\d*|\.?\d+)(?:[eE][-+]?\d+)?/g;
var reB = new RegExp(reA.source, "g");

function zero(b) {
  return function() {
    return b;
  };
}

function one(b) {
  return function(t) {
    return b(t) + "";
  };
}

var string = function(a, b) {
  var bi = reA.lastIndex = reB.lastIndex = 0, // scan index for next number in b
      am, // current match in a
      bm, // current match in b
      bs, // string preceding current number in b, if any
      i = -1, // index in s
      s = [], // string constants and placeholders
      q = []; // number interpolators

  // Coerce inputs to strings.
  a = a + "", b = b + "";

  // Interpolate pairs of numbers in a & b.
  while ((am = reA.exec(a))
      && (bm = reB.exec(b))) {
    if ((bs = bm.index) > bi) { // a string precedes the next number in b
      bs = b.slice(bi, bs);
      if (s[i]) s[i] += bs; // coalesce with previous string
      else s[++i] = bs;
    }
    if ((am = am[0]) === (bm = bm[0])) { // numbers in a & b match
      if (s[i]) s[i] += bm; // coalesce with previous string
      else s[++i] = bm;
    } else { // interpolate non-matching numbers
      s[++i] = null;
      q.push({i: i, x: reinterpolate(am, bm)});
    }
    bi = reB.lastIndex;
  }

  // Add remains of b.
  if (bi < b.length) {
    bs = b.slice(bi);
    if (s[i]) s[i] += bs; // coalesce with previous string
    else s[++i] = bs;
  }

  // Special optimization for only a single match.
  // Otherwise, interpolate each of the numbers and rejoin the string.
  return s.length < 2 ? (q[0]
      ? one(q[0].x)
      : zero(b))
      : (b = q.length, function(t) {
          for (var i = 0, o; i < b; ++i) s[(o = q[i]).i] = o.x(t);
          return s.join("");
        });
};

var interpolateValue = function(a, b) {
  var t = typeof b, c;
  return b == null || t === "boolean" ? constant$1(b)
      : (t === "number" ? reinterpolate
      : t === "string" ? ((c = color(b)) ? (b = c, rgb$1) : string)
      : b instanceof color ? rgb$1
      : b instanceof Date ? date
      : Array.isArray(b) ? array$1
      : isNaN(b) ? object
      : reinterpolate)(a, b);
};

var interpolateRound = function(a, b) {
  return a = +a, b -= a, function(t) {
    return Math.round(a + b * t);
  };
};

var degrees = 180 / Math.PI;

var identity$2 = {
  translateX: 0,
  translateY: 0,
  rotate: 0,
  skewX: 0,
  scaleX: 1,
  scaleY: 1
};

var decompose = function(a, b, c, d, e, f) {
  var scaleX, scaleY, skewX;
  if (scaleX = Math.sqrt(a * a + b * b)) a /= scaleX, b /= scaleX;
  if (skewX = a * c + b * d) c -= a * skewX, d -= b * skewX;
  if (scaleY = Math.sqrt(c * c + d * d)) c /= scaleY, d /= scaleY, skewX /= scaleY;
  if (a * d < b * c) a = -a, b = -b, skewX = -skewX, scaleX = -scaleX;
  return {
    translateX: e,
    translateY: f,
    rotate: Math.atan2(b, a) * degrees,
    skewX: Math.atan(skewX) * degrees,
    scaleX: scaleX,
    scaleY: scaleY
  };
};

var cssNode;
var cssRoot;
var cssView;
var svgNode;

var rho = Math.SQRT2;
var rho2 = 2;
var rho4 = 4;
var epsilon2 = 1e-12;

function cosh(x) {
  return ((x = Math.exp(x)) + 1 / x) / 2;
}

function sinh(x) {
  return ((x = Math.exp(x)) - 1 / x) / 2;
}

function tanh(x) {
  return ((x = Math.exp(2 * x)) - 1) / (x + 1);
}

// p0 = [ux0, uy0, w0]
// p1 = [ux1, uy1, w1]

function hcl$1(hue$$1) {
  return function(start, end) {
    var h = hue$$1((start = hcl(start)).h, (end = hcl(end)).h),
        c = nogamma(start.c, end.c),
        l = nogamma(start.l, end.l),
        opacity = nogamma(start.opacity, end.opacity);
    return function(t) {
      start.h = h(t);
      start.c = c(t);
      start.l = l(t);
      start.opacity = opacity(t);
      return start + "";
    };
  }
}

var interpolateHcl = hcl$1(hue);

function cubehelix$1(hue$$1) {
  return (function cubehelixGamma(y) {
    y = +y;

    function cubehelix$$1(start, end) {
      var h = hue$$1((start = cubehelix(start)).h, (end = cubehelix(end)).h),
          s = nogamma(start.s, end.s),
          l = nogamma(start.l, end.l),
          opacity = nogamma(start.opacity, end.opacity);
      return function(t) {
        start.h = h(t);
        start.s = s(t);
        start.l = l(Math.pow(t, y));
        start.opacity = opacity(t);
        return start + "";
      };
    }

    cubehelix$$1.gamma = cubehelixGamma;

    return cubehelix$$1;
  })(1);
}

cubehelix$1(hue);
var cubehelixLong = cubehelix$1(nogamma);

var noop = {value: function() {}};

function dispatch() {
  for (var i = 0, n = arguments.length, _ = {}, t; i < n; ++i) {
    if (!(t = arguments[i] + "") || (t in _)) throw new Error("illegal type: " + t);
    _[t] = [];
  }
  return new Dispatch(_);
}

function Dispatch(_) {
  this._ = _;
}

function parseTypenames(typenames, types) {
  return typenames.trim().split(/^|\s+/).map(function(t) {
    var name = "", i = t.indexOf(".");
    if (i >= 0) name = t.slice(i + 1), t = t.slice(0, i);
    if (t && !types.hasOwnProperty(t)) throw new Error("unknown type: " + t);
    return {type: t, name: name};
  });
}

Dispatch.prototype = dispatch.prototype = {
  constructor: Dispatch,
  on: function(typename, callback) {
    var _ = this._,
        T = parseTypenames(typename + "", _),
        t,
        i = -1,
        n = T.length;

    // If no callback was specified, return the callback of the given type and name.
    if (arguments.length < 2) {
      while (++i < n) if ((t = (typename = T[i]).type) && (t = get(_[t], typename.name))) return t;
      return;
    }

    // If a type was specified, set the callback for the given type and name.
    // Otherwise, if a null callback was specified, remove callbacks of the given name.
    if (callback != null && typeof callback !== "function") throw new Error("invalid callback: " + callback);
    while (++i < n) {
      if (t = (typename = T[i]).type) _[t] = set$2(_[t], typename.name, callback);
      else if (callback == null) for (t in _) _[t] = set$2(_[t], typename.name, null);
    }

    return this;
  },
  copy: function() {
    var copy = {}, _ = this._;
    for (var t in _) copy[t] = _[t].slice();
    return new Dispatch(copy);
  },
  call: function(type, that) {
    if ((n = arguments.length - 2) > 0) for (var args = new Array(n), i = 0, n, t; i < n; ++i) args[i] = arguments[i + 2];
    if (!this._.hasOwnProperty(type)) throw new Error("unknown type: " + type);
    for (t = this._[type], i = 0, n = t.length; i < n; ++i) t[i].value.apply(that, args);
  },
  apply: function(type, that, args) {
    if (!this._.hasOwnProperty(type)) throw new Error("unknown type: " + type);
    for (var t = this._[type], i = 0, n = t.length; i < n; ++i) t[i].value.apply(that, args);
  }
};

function get(type, name) {
  for (var i = 0, n = type.length, c; i < n; ++i) {
    if ((c = type[i]).name === name) {
      return c.value;
    }
  }
}

function set$2(type, name, callback) {
  for (var i = 0, n = type.length; i < n; ++i) {
    if (type[i].name === name) {
      type[i] = noop, type = type.slice(0, i).concat(type.slice(i + 1));
      break;
    }
  }
  if (callback != null) type.push({name: name, value: callback});
  return type;
}

var request = function(url, callback) {
  var request,
      event = dispatch("beforesend", "progress", "load", "error"),
      mimeType,
      headers = map$1(),
      xhr = new XMLHttpRequest,
      user = null,
      password = null,
      response,
      responseType,
      timeout = 0;

  // If IE does not support CORS, use XDomainRequest.
  if (typeof XDomainRequest !== "undefined"
      && !("withCredentials" in xhr)
      && /^(http(s)?:)?\/\//.test(url)) xhr = new XDomainRequest;

  "onload" in xhr
      ? xhr.onload = xhr.onerror = xhr.ontimeout = respond
      : xhr.onreadystatechange = function(o) { xhr.readyState > 3 && respond(o); };

  function respond(o) {
    var status = xhr.status, result;
    if (!status && hasResponse(xhr)
        || status >= 200 && status < 300
        || status === 304) {
      if (response) {
        try {
          result = response.call(request, xhr);
        } catch (e) {
          event.call("error", request, e);
          return;
        }
      } else {
        result = xhr;
      }
      event.call("load", request, result);
    } else {
      event.call("error", request, o);
    }
  }

  xhr.onprogress = function(e) {
    event.call("progress", request, e);
  };

  request = {
    header: function(name, value) {
      name = (name + "").toLowerCase();
      if (arguments.length < 2) return headers.get(name);
      if (value == null) headers.remove(name);
      else headers.set(name, value + "");
      return request;
    },

    // If mimeType is non-null and no Accept header is set, a default is used.
    mimeType: function(value) {
      if (!arguments.length) return mimeType;
      mimeType = value == null ? null : value + "";
      return request;
    },

    // Specifies what type the response value should take;
    // for instance, arraybuffer, blob, document, or text.
    responseType: function(value) {
      if (!arguments.length) return responseType;
      responseType = value;
      return request;
    },

    timeout: function(value) {
      if (!arguments.length) return timeout;
      timeout = +value;
      return request;
    },

    user: function(value) {
      return arguments.length < 1 ? user : (user = value == null ? null : value + "", request);
    },

    password: function(value) {
      return arguments.length < 1 ? password : (password = value == null ? null : value + "", request);
    },

    // Specify how to convert the response content to a specific type;
    // changes the callback value on "load" events.
    response: function(value) {
      response = value;
      return request;
    },

    // Alias for send("GET", …).
    get: function(data, callback) {
      return request.send("GET", data, callback);
    },

    // Alias for send("POST", …).
    post: function(data, callback) {
      return request.send("POST", data, callback);
    },

    // If callback is non-null, it will be used for error and load events.
    send: function(method, data, callback) {
      xhr.open(method, url, true, user, password);
      if (mimeType != null && !headers.has("accept")) headers.set("accept", mimeType + ",*/*");
      if (xhr.setRequestHeader) headers.each(function(value, name) { xhr.setRequestHeader(name, value); });
      if (mimeType != null && xhr.overrideMimeType) xhr.overrideMimeType(mimeType);
      if (responseType != null) xhr.responseType = responseType;
      if (timeout > 0) xhr.timeout = timeout;
      if (callback == null && typeof data === "function") callback = data, data = null;
      if (callback != null && callback.length === 1) callback = fixCallback(callback);
      if (callback != null) request.on("error", callback).on("load", function(xhr) { callback(null, xhr); });
      event.call("beforesend", request, xhr);
      xhr.send(data == null ? null : data);
      return request;
    },

    abort: function() {
      xhr.abort();
      return request;
    },

    on: function() {
      var value = event.on.apply(event, arguments);
      return value === event ? request : value;
    }
  };

  if (callback != null) {
    if (typeof callback !== "function") throw new Error("invalid callback: " + callback);
    return request.get(callback);
  }

  return request;
};

function fixCallback(callback) {
  return function(error, xhr) {
    callback(error == null ? xhr : null);
  };
}

function hasResponse(xhr) {
  var type = xhr.responseType;
  return type && type !== "text"
      ? xhr.response // null on error
      : xhr.responseText; // "" on error
}

var type = function(defaultMimeType, response) {
  return function(url, callback) {
    var r = request(url).mimeType(defaultMimeType).response(response);
    if (callback != null) {
      if (typeof callback !== "function") throw new Error("invalid callback: " + callback);
      return r.get(callback);
    }
    return r;
  };
};

type("text/html", function(xhr) {
  return document.createRange().createContextualFragment(xhr.responseText);
});

var json = type("application/json", function(xhr) {
  return JSON.parse(xhr.responseText);
});

var text = type("text/plain", function(xhr) {
  return xhr.responseText;
});

type("application/xml", function(xhr) {
  var xml = xhr.responseXML;
  if (!xml) throw new Error("parse error");
  return xml;
});

function objectConverter(columns) {
  return new Function("d", "return {" + columns.map(function(name, i) {
    return JSON.stringify(name) + ": d[" + i + "]";
  }).join(",") + "}");
}

function customConverter(columns, f) {
  var object = objectConverter(columns);
  return function(row, i) {
    return f(object(row), i, columns);
  };
}

// Compute unique columns in order of discovery.
function inferColumns(rows) {
  var columnSet = Object.create(null),
      columns = [];

  rows.forEach(function(row) {
    for (var column in row) {
      if (!(column in columnSet)) {
        columns.push(columnSet[column] = column);
      }
    }
  });

  return columns;
}

var dsv = function(delimiter) {
  var reFormat = new RegExp("[\"" + delimiter + "\n]"),
      delimiterCode = delimiter.charCodeAt(0);

  function parse(text, f) {
    var convert, columns, rows = parseRows(text, function(row, i) {
      if (convert) return convert(row, i - 1);
      columns = row, convert = f ? customConverter(row, f) : objectConverter(row);
    });
    rows.columns = columns;
    return rows;
  }

  function parseRows(text, f) {
    var EOL = {}, // sentinel value for end-of-line
        EOF = {}, // sentinel value for end-of-file
        rows = [], // output rows
        N = text.length,
        I = 0, // current character index
        n = 0, // the current line number
        t, // the current token
        eol; // is the current token followed by EOL?

    function token() {
      if (I >= N) return EOF; // special case: end of file
      if (eol) return eol = false, EOL; // special case: end of line

      // special case: quotes
      var j = I, c;
      if (text.charCodeAt(j) === 34) {
        var i = j;
        while (i++ < N) {
          if (text.charCodeAt(i) === 34) {
            if (text.charCodeAt(i + 1) !== 34) break;
            ++i;
          }
        }
        I = i + 2;
        c = text.charCodeAt(i + 1);
        if (c === 13) {
          eol = true;
          if (text.charCodeAt(i + 2) === 10) ++I;
        } else if (c === 10) {
          eol = true;
        }
        return text.slice(j + 1, i).replace(/""/g, "\"");
      }

      // common case: find next delimiter or newline
      while (I < N) {
        var k = 1;
        c = text.charCodeAt(I++);
        if (c === 10) eol = true; // \n
        else if (c === 13) { eol = true; if (text.charCodeAt(I) === 10) ++I, ++k; } // \r|\r\n
        else if (c !== delimiterCode) continue;
        return text.slice(j, I - k);
      }

      // special case: last token before EOF
      return text.slice(j);
    }

    while ((t = token()) !== EOF) {
      var a = [];
      while (t !== EOL && t !== EOF) {
        a.push(t);
        t = token();
      }
      if (f && (a = f(a, n++)) == null) continue;
      rows.push(a);
    }

    return rows;
  }

  function format(rows, columns) {
    if (columns == null) columns = inferColumns(rows);
    return [columns.map(formatValue).join(delimiter)].concat(rows.map(function(row) {
      return columns.map(function(column) {
        return formatValue(row[column]);
      }).join(delimiter);
    })).join("\n");
  }

  function formatRows(rows) {
    return rows.map(formatRow).join("\n");
  }

  function formatRow(row) {
    return row.map(formatValue).join(delimiter);
  }

  function formatValue(text) {
    return text == null ? ""
        : reFormat.test(text += "") ? "\"" + text.replace(/\"/g, "\"\"") + "\""
        : text;
  }

  return {
    parse: parse,
    parseRows: parseRows,
    format: format,
    formatRows: formatRows
  };
};

var csv$1 = dsv(",");

var csvParse = csv$1.parse;

var tsv = dsv("\t");

var tsvParse = tsv.parse;

var dsv$1 = function(defaultMimeType, parse) {
  return function(url, row, callback) {
    if (arguments.length < 3) callback = row, row = null;
    var r = request(url).mimeType(defaultMimeType);
    r.row = function(_) { return arguments.length ? r.response(responseOf(parse, row = _)) : row; };
    r.row(row);
    return callback ? r.get(callback) : r;
  };
};

function responseOf(parse, row) {
  return function(request$$1) {
    return parse(request$$1.responseText, row);
  };
}

var csv = dsv$1("text/csv", csvParse);

var tsv$1 = dsv$1("text/tab-separated-values", tsvParse);

var array$2 = Array.prototype;

var map$3 = array$2.map;
var slice$1 = array$2.slice;

var implicit = {name: "implicit"};

function ordinal(range) {
  var index = map$1(),
      domain = [],
      unknown = implicit;

  range = range == null ? [] : slice$1.call(range);

  function scale(d) {
    var key = d + "", i = index.get(key);
    if (!i) {
      if (unknown !== implicit) return unknown;
      index.set(key, i = domain.push(d));
    }
    return range[(i - 1) % range.length];
  }

  scale.domain = function(_) {
    if (!arguments.length) return domain.slice();
    domain = [], index = map$1();
    var i = -1, n = _.length, d, key;
    while (++i < n) if (!index.has(key = (d = _[i]) + "")) index.set(key, domain.push(d));
    return scale;
  };

  scale.range = function(_) {
    return arguments.length ? (range = slice$1.call(_), scale) : range.slice();
  };

  scale.unknown = function(_) {
    return arguments.length ? (unknown = _, scale) : unknown;
  };

  scale.copy = function() {
    return ordinal()
        .domain(domain)
        .range(range)
        .unknown(unknown);
  };

  return scale;
}

function band() {
  var scale = ordinal().unknown(undefined),
      domain = scale.domain,
      ordinalRange = scale.range,
      range$$1 = [0, 1],
      step,
      bandwidth,
      round = false,
      paddingInner = 0,
      paddingOuter = 0,
      align = 0.5;

  delete scale.unknown;

  function rescale() {
    var n = domain().length,
        reverse = range$$1[1] < range$$1[0],
        start = range$$1[reverse - 0],
        stop = range$$1[1 - reverse];
    step = (stop - start) / Math.max(1, n - paddingInner + paddingOuter * 2);
    if (round) step = Math.floor(step);
    start += (stop - start - step * (n - paddingInner)) * align;
    bandwidth = step * (1 - paddingInner);
    if (round) start = Math.round(start), bandwidth = Math.round(bandwidth);
    var values = sequence(n).map(function(i) { return start + step * i; });
    return ordinalRange(reverse ? values.reverse() : values);
  }

  scale.domain = function(_) {
    return arguments.length ? (domain(_), rescale()) : domain();
  };

  scale.range = function(_) {
    return arguments.length ? (range$$1 = [+_[0], +_[1]], rescale()) : range$$1.slice();
  };

  scale.rangeRound = function(_) {
    return range$$1 = [+_[0], +_[1]], round = true, rescale();
  };

  scale.bandwidth = function() {
    return bandwidth;
  };

  scale.step = function() {
    return step;
  };

  scale.round = function(_) {
    return arguments.length ? (round = !!_, rescale()) : round;
  };

  scale.padding = function(_) {
    return arguments.length ? (paddingInner = paddingOuter = Math.max(0, Math.min(1, _)), rescale()) : paddingInner;
  };

  scale.paddingInner = function(_) {
    return arguments.length ? (paddingInner = Math.max(0, Math.min(1, _)), rescale()) : paddingInner;
  };

  scale.paddingOuter = function(_) {
    return arguments.length ? (paddingOuter = Math.max(0, Math.min(1, _)), rescale()) : paddingOuter;
  };

  scale.align = function(_) {
    return arguments.length ? (align = Math.max(0, Math.min(1, _)), rescale()) : align;
  };

  scale.copy = function() {
    return band()
        .domain(domain())
        .range(range$$1)
        .round(round)
        .paddingInner(paddingInner)
        .paddingOuter(paddingOuter)
        .align(align);
  };

  return rescale();
}

function pointish(scale) {
  var copy = scale.copy;

  scale.padding = scale.paddingOuter;
  delete scale.paddingInner;
  delete scale.paddingOuter;

  scale.copy = function() {
    return pointish(copy());
  };

  return scale;
}

var constant$2 = function(x) {
  return function() {
    return x;
  };
};

var number$1 = function(x) {
  return +x;
};

var unit = [0, 1];

function deinterpolateLinear(a, b) {
  return (b -= (a = +a))
      ? function(x) { return (x - a) / b; }
      : constant$2(b);
}

function deinterpolateClamp(deinterpolate) {
  return function(a, b) {
    var d = deinterpolate(a = +a, b = +b);
    return function(x) { return x <= a ? 0 : x >= b ? 1 : d(x); };
  };
}

function reinterpolateClamp(reinterpolate) {
  return function(a, b) {
    var r = reinterpolate(a = +a, b = +b);
    return function(t) { return t <= 0 ? a : t >= 1 ? b : r(t); };
  };
}

function bimap(domain, range$$1, deinterpolate, reinterpolate) {
  var d0 = domain[0], d1 = domain[1], r0 = range$$1[0], r1 = range$$1[1];
  if (d1 < d0) d0 = deinterpolate(d1, d0), r0 = reinterpolate(r1, r0);
  else d0 = deinterpolate(d0, d1), r0 = reinterpolate(r0, r1);
  return function(x) { return r0(d0(x)); };
}

function polymap(domain, range$$1, deinterpolate, reinterpolate) {
  var j = Math.min(domain.length, range$$1.length) - 1,
      d = new Array(j),
      r = new Array(j),
      i = -1;

  // Reverse descending domains.
  if (domain[j] < domain[0]) {
    domain = domain.slice().reverse();
    range$$1 = range$$1.slice().reverse();
  }

  while (++i < j) {
    d[i] = deinterpolate(domain[i], domain[i + 1]);
    r[i] = reinterpolate(range$$1[i], range$$1[i + 1]);
  }

  return function(x) {
    var i = bisectRight(domain, x, 1, j) - 1;
    return r[i](d[i](x));
  };
}

function copy(source, target) {
  return target
      .domain(source.domain())
      .range(source.range())
      .interpolate(source.interpolate())
      .clamp(source.clamp());
}

// deinterpolate(a, b)(x) takes a domain value x in [a,b] and returns the corresponding parameter t in [0,1].
// reinterpolate(a, b)(t) takes a parameter t in [0,1] and returns the corresponding domain value x in [a,b].
function continuous(deinterpolate, reinterpolate) {
  var domain = unit,
      range$$1 = unit,
      interpolate$$1 = interpolateValue,
      clamp = false,
      piecewise,
      output,
      input;

  function rescale() {
    piecewise = Math.min(domain.length, range$$1.length) > 2 ? polymap : bimap;
    output = input = null;
    return scale;
  }

  function scale(x) {
    return (output || (output = piecewise(domain, range$$1, clamp ? deinterpolateClamp(deinterpolate) : deinterpolate, interpolate$$1)))(+x);
  }

  scale.invert = function(y) {
    return (input || (input = piecewise(range$$1, domain, deinterpolateLinear, clamp ? reinterpolateClamp(reinterpolate) : reinterpolate)))(+y);
  };

  scale.domain = function(_) {
    return arguments.length ? (domain = map$3.call(_, number$1), rescale()) : domain.slice();
  };

  scale.range = function(_) {
    return arguments.length ? (range$$1 = slice$1.call(_), rescale()) : range$$1.slice();
  };

  scale.rangeRound = function(_) {
    return range$$1 = slice$1.call(_), interpolate$$1 = interpolateRound, rescale();
  };

  scale.clamp = function(_) {
    return arguments.length ? (clamp = !!_, rescale()) : clamp;
  };

  scale.interpolate = function(_) {
    return arguments.length ? (interpolate$$1 = _, rescale()) : interpolate$$1;
  };

  return rescale();
}

var tickFormat = function(domain, count, specifier) {
  var start = domain[0],
      stop = domain[domain.length - 1],
      step = tickStep(start, stop, count == null ? 10 : count),
      precision;
  specifier = formatSpecifier(specifier == null ? ",f" : specifier);
  switch (specifier.type) {
    case "s": {
      var value = Math.max(Math.abs(start), Math.abs(stop));
      if (specifier.precision == null && !isNaN(precision = precisionPrefix(step, value))) specifier.precision = precision;
      return formatPrefix(specifier, value);
    }
    case "":
    case "e":
    case "g":
    case "p":
    case "r": {
      if (specifier.precision == null && !isNaN(precision = precisionRound(step, Math.max(Math.abs(start), Math.abs(stop))))) specifier.precision = precision - (specifier.type === "e");
      break;
    }
    case "f":
    case "%": {
      if (specifier.precision == null && !isNaN(precision = precisionFixed(step))) specifier.precision = precision - (specifier.type === "%") * 2;
      break;
    }
  }
  return format(specifier);
};

function linearish(scale) {
  var domain = scale.domain;

  scale.ticks = function(count) {
    var d = domain();
    return ticks(d[0], d[d.length - 1], count == null ? 10 : count);
  };

  scale.tickFormat = function(count, specifier) {
    return tickFormat(domain(), count, specifier);
  };

  scale.nice = function(count) {
    var d = domain(),
        i = d.length - 1,
        n = count == null ? 10 : count,
        start = d[0],
        stop = d[i],
        step = tickStep(start, stop, n);

    if (step) {
      step = tickStep(Math.floor(start / step) * step, Math.ceil(stop / step) * step, n);
      d[0] = Math.floor(start / step) * step;
      d[i] = Math.ceil(stop / step) * step;
      domain(d);
    }

    return scale;
  };

  return scale;
}

function linear$1() {
  var scale = continuous(deinterpolateLinear, reinterpolate);

  scale.copy = function() {
    return copy(scale, linear$1());
  };

  return linearish(scale);
}

function identity$3() {
  var domain = [0, 1];

  function scale(x) {
    return +x;
  }

  scale.invert = scale;

  scale.domain = scale.range = function(_) {
    return arguments.length ? (domain = map$3.call(_, number$1), scale) : domain.slice();
  };

  scale.copy = function() {
    return identity$3().domain(domain);
  };

  return linearish(scale);
}

var nice = function(domain, interval) {
  domain = domain.slice();

  var i0 = 0,
      i1 = domain.length - 1,
      x0 = domain[i0],
      x1 = domain[i1],
      t;

  if (x1 < x0) {
    t = i0, i0 = i1, i1 = t;
    t = x0, x0 = x1, x1 = t;
  }

  domain[i0] = interval.floor(x0);
  domain[i1] = interval.ceil(x1);
  return domain;
};

function deinterpolate(a, b) {
  return (b = Math.log(b / a))
      ? function(x) { return Math.log(x / a) / b; }
      : constant$2(b);
}

function reinterpolate$1(a, b) {
  return a < 0
      ? function(t) { return -Math.pow(-b, t) * Math.pow(-a, 1 - t); }
      : function(t) { return Math.pow(b, t) * Math.pow(a, 1 - t); };
}

function pow10(x) {
  return isFinite(x) ? +("1e" + x) : x < 0 ? 0 : x;
}

function powp(base) {
  return base === 10 ? pow10
      : base === Math.E ? Math.exp
      : function(x) { return Math.pow(base, x); };
}

function logp(base) {
  return base === Math.E ? Math.log
      : base === 10 && Math.log10
      || base === 2 && Math.log2
      || (base = Math.log(base), function(x) { return Math.log(x) / base; });
}

function reflect(f) {
  return function(x) {
    return -f(-x);
  };
}

function log() {
  var scale = continuous(deinterpolate, reinterpolate$1).domain([1, 10]),
      domain = scale.domain,
      base = 10,
      logs = logp(10),
      pows = powp(10);

  function rescale() {
    logs = logp(base), pows = powp(base);
    if (domain()[0] < 0) logs = reflect(logs), pows = reflect(pows);
    return scale;
  }

  scale.base = function(_) {
    return arguments.length ? (base = +_, rescale()) : base;
  };

  scale.domain = function(_) {
    return arguments.length ? (domain(_), rescale()) : domain();
  };

  scale.ticks = function(count) {
    var d = domain(),
        u = d[0],
        v = d[d.length - 1],
        r;

    if (r = v < u) i = u, u = v, v = i;

    var i = logs(u),
        j = logs(v),
        p,
        k,
        t,
        n = count == null ? 10 : +count,
        z = [];

    if (!(base % 1) && j - i < n) {
      i = Math.round(i) - 1, j = Math.round(j) + 1;
      if (u > 0) for (; i < j; ++i) {
        for (k = 1, p = pows(i); k < base; ++k) {
          t = p * k;
          if (t < u) continue;
          if (t > v) break;
          z.push(t);
        }
      } else for (; i < j; ++i) {
        for (k = base - 1, p = pows(i); k >= 1; --k) {
          t = p * k;
          if (t < u) continue;
          if (t > v) break;
          z.push(t);
        }
      }
    } else {
      z = ticks(i, j, Math.min(j - i, n)).map(pows);
    }

    return r ? z.reverse() : z;
  };

  scale.tickFormat = function(count, specifier) {
    if (specifier == null) specifier = base === 10 ? ".0e" : ",";
    if (typeof specifier !== "function") specifier = format(specifier);
    if (count === Infinity) return specifier;
    if (count == null) count = 10;
    var k = Math.max(1, base * count / scale.ticks().length); // TODO fast estimate?
    return function(d) {
      var i = d / pows(Math.round(logs(d)));
      if (i * base < base - 0.5) i *= base;
      return i <= k ? specifier(d) : "";
    };
  };

  scale.nice = function() {
    return domain(nice(domain(), {
      floor: function(x) { return pows(Math.floor(logs(x))); },
      ceil: function(x) { return pows(Math.ceil(logs(x))); }
    }));
  };

  scale.copy = function() {
    return copy(scale, log().base(base));
  };

  return scale;
}

function raise(x, exponent) {
  return x < 0 ? -Math.pow(-x, exponent) : Math.pow(x, exponent);
}

function pow() {
  var exponent = 1,
      scale = continuous(deinterpolate, reinterpolate),
      domain = scale.domain;

  function deinterpolate(a, b) {
    return (b = raise(b, exponent) - (a = raise(a, exponent)))
        ? function(x) { return (raise(x, exponent) - a) / b; }
        : constant$2(b);
  }

  function reinterpolate(a, b) {
    b = raise(b, exponent) - (a = raise(a, exponent));
    return function(t) { return raise(a + b * t, 1 / exponent); };
  }

  scale.exponent = function(_) {
    return arguments.length ? (exponent = +_, domain(domain())) : exponent;
  };

  scale.copy = function() {
    return copy(scale, pow().exponent(exponent));
  };

  return linearish(scale);
}

function quantile$$1() {
  var domain = [],
      range$$1 = [],
      thresholds = [];

  function rescale() {
    var i = 0, n = Math.max(1, range$$1.length);
    thresholds = new Array(n - 1);
    while (++i < n) thresholds[i - 1] = threshold(domain, i / n);
    return scale;
  }

  function scale(x) {
    if (!isNaN(x = +x)) return range$$1[bisectRight(thresholds, x)];
  }

  scale.invertExtent = function(y) {
    var i = range$$1.indexOf(y);
    return i < 0 ? [NaN, NaN] : [
      i > 0 ? thresholds[i - 1] : domain[0],
      i < thresholds.length ? thresholds[i] : domain[domain.length - 1]
    ];
  };

  scale.domain = function(_) {
    if (!arguments.length) return domain.slice();
    domain = [];
    for (var i = 0, n = _.length, d; i < n; ++i) if (d = _[i], d != null && !isNaN(d = +d)) domain.push(d);
    domain.sort(ascending);
    return rescale();
  };

  scale.range = function(_) {
    return arguments.length ? (range$$1 = slice$1.call(_), rescale()) : range$$1.slice();
  };

  scale.quantiles = function() {
    return thresholds.slice();
  };

  scale.copy = function() {
    return quantile$$1()
        .domain(domain)
        .range(range$$1);
  };

  return scale;
}

function quantize$1() {
  var x0 = 0,
      x1 = 1,
      n = 1,
      domain = [0.5],
      range$$1 = [0, 1];

  function scale(x) {
    if (x <= x) return range$$1[bisectRight(domain, x, 0, n)];
  }

  function rescale() {
    var i = -1;
    domain = new Array(n);
    while (++i < n) domain[i] = ((i + 1) * x1 - (i - n) * x0) / (n + 1);
    return scale;
  }

  scale.domain = function(_) {
    return arguments.length ? (x0 = +_[0], x1 = +_[1], rescale()) : [x0, x1];
  };

  scale.range = function(_) {
    return arguments.length ? (n = (range$$1 = slice$1.call(_)).length - 1, rescale()) : range$$1.slice();
  };

  scale.invertExtent = function(y) {
    var i = range$$1.indexOf(y);
    return i < 0 ? [NaN, NaN]
        : i < 1 ? [x0, domain[0]]
        : i >= n ? [domain[n - 1], x1]
        : [domain[i - 1], domain[i]];
  };

  scale.copy = function() {
    return quantize$1()
        .domain([x0, x1])
        .range(range$$1);
  };

  return linearish(scale);
}

function threshold$1() {
  var domain = [0.5],
      range$$1 = [0, 1],
      n = 1;

  function scale(x) {
    if (x <= x) return range$$1[bisectRight(domain, x, 0, n)];
  }

  scale.domain = function(_) {
    return arguments.length ? (domain = slice$1.call(_), n = Math.min(domain.length, range$$1.length - 1), scale) : domain.slice();
  };

  scale.range = function(_) {
    return arguments.length ? (range$$1 = slice$1.call(_), n = Math.min(domain.length, range$$1.length - 1), scale) : range$$1.slice();
  };

  scale.invertExtent = function(y) {
    var i = range$$1.indexOf(y);
    return [domain[i - 1], domain[i]];
  };

  scale.copy = function() {
    return threshold$1()
        .domain(domain)
        .range(range$$1);
  };

  return scale;
}

var t0$1 = new Date;
var t1$1 = new Date;

function newInterval(floori, offseti, count, field) {

  function interval(date) {
    return floori(date = new Date(+date)), date;
  }

  interval.floor = interval;

  interval.ceil = function(date) {
    return floori(date = new Date(date - 1)), offseti(date, 1), floori(date), date;
  };

  interval.round = function(date) {
    var d0 = interval(date),
        d1 = interval.ceil(date);
    return date - d0 < d1 - date ? d0 : d1;
  };

  interval.offset = function(date, step) {
    return offseti(date = new Date(+date), step == null ? 1 : Math.floor(step)), date;
  };

  interval.range = function(start, stop, step) {
    var range = [];
    start = interval.ceil(start);
    step = step == null ? 1 : Math.floor(step);
    if (!(start < stop) || !(step > 0)) return range; // also handles Invalid Date
    do range.push(new Date(+start)); while (offseti(start, step), floori(start), start < stop)
    return range;
  };

  interval.filter = function(test) {
    return newInterval(function(date) {
      if (date >= date) while (floori(date), !test(date)) date.setTime(date - 1);
    }, function(date, step) {
      if (date >= date) while (--step >= 0) while (offseti(date, 1), !test(date)) {} // eslint-disable-line no-empty
    });
  };

  if (count) {
    interval.count = function(start, end) {
      t0$1.setTime(+start), t1$1.setTime(+end);
      floori(t0$1), floori(t1$1);
      return Math.floor(count(t0$1, t1$1));
    };

    interval.every = function(step) {
      step = Math.floor(step);
      return !isFinite(step) || !(step > 0) ? null
          : !(step > 1) ? interval
          : interval.filter(field
              ? function(d) { return field(d) % step === 0; }
              : function(d) { return interval.count(0, d) % step === 0; });
    };
  }

  return interval;
}

var millisecond = newInterval(function() {
  // noop
}, function(date, step) {
  date.setTime(+date + step);
}, function(start, end) {
  return end - start;
});

// An optimized implementation for this simple case.
millisecond.every = function(k) {
  k = Math.floor(k);
  if (!isFinite(k) || !(k > 0)) return null;
  if (!(k > 1)) return millisecond;
  return newInterval(function(date) {
    date.setTime(Math.floor(date / k) * k);
  }, function(date, step) {
    date.setTime(+date + step * k);
  }, function(start, end) {
    return (end - start) / k;
  });
};

var durationSecond$1 = 1e3;
var durationMinute$1 = 6e4;
var durationHour$1 = 36e5;
var durationDay$1 = 864e5;
var durationWeek$1 = 6048e5;

var second = newInterval(function(date) {
  date.setTime(Math.floor(date / durationSecond$1) * durationSecond$1);
}, function(date, step) {
  date.setTime(+date + step * durationSecond$1);
}, function(start, end) {
  return (end - start) / durationSecond$1;
}, function(date) {
  return date.getUTCSeconds();
});

var minute = newInterval(function(date) {
  date.setTime(Math.floor(date / durationMinute$1) * durationMinute$1);
}, function(date, step) {
  date.setTime(+date + step * durationMinute$1);
}, function(start, end) {
  return (end - start) / durationMinute$1;
}, function(date) {
  return date.getMinutes();
});

var minutes = minute.range;

var hour = newInterval(function(date) {
  var offset = date.getTimezoneOffset() * durationMinute$1 % durationHour$1;
  if (offset < 0) offset += durationHour$1;
  date.setTime(Math.floor((+date - offset) / durationHour$1) * durationHour$1 + offset);
}, function(date, step) {
  date.setTime(+date + step * durationHour$1);
}, function(start, end) {
  return (end - start) / durationHour$1;
}, function(date) {
  return date.getHours();
});

var hours = hour.range;

var day = newInterval(function(date) {
  date.setHours(0, 0, 0, 0);
}, function(date, step) {
  date.setDate(date.getDate() + step);
}, function(start, end) {
  return (end - start - (end.getTimezoneOffset() - start.getTimezoneOffset()) * durationMinute$1) / durationDay$1;
}, function(date) {
  return date.getDate() - 1;
});

var days = day.range;

function weekday(i) {
  return newInterval(function(date) {
    date.setDate(date.getDate() - (date.getDay() + 7 - i) % 7);
    date.setHours(0, 0, 0, 0);
  }, function(date, step) {
    date.setDate(date.getDate() + step * 7);
  }, function(start, end) {
    return (end - start - (end.getTimezoneOffset() - start.getTimezoneOffset()) * durationMinute$1) / durationWeek$1;
  });
}

var sunday = weekday(0);
var monday = weekday(1);
var tuesday = weekday(2);
var wednesday = weekday(3);
var thursday = weekday(4);
var friday = weekday(5);
var saturday = weekday(6);

var sundays = sunday.range;
var mondays = monday.range;

var month = newInterval(function(date) {
  date.setDate(1);
  date.setHours(0, 0, 0, 0);
}, function(date, step) {
  date.setMonth(date.getMonth() + step);
}, function(start, end) {
  return end.getMonth() - start.getMonth() + (end.getFullYear() - start.getFullYear()) * 12;
}, function(date) {
  return date.getMonth();
});

var months = month.range;

var year = newInterval(function(date) {
  date.setMonth(0, 1);
  date.setHours(0, 0, 0, 0);
}, function(date, step) {
  date.setFullYear(date.getFullYear() + step);
}, function(start, end) {
  return end.getFullYear() - start.getFullYear();
}, function(date) {
  return date.getFullYear();
});

// An optimized implementation for this simple case.
year.every = function(k) {
  return !isFinite(k = Math.floor(k)) || !(k > 0) ? null : newInterval(function(date) {
    date.setFullYear(Math.floor(date.getFullYear() / k) * k);
    date.setMonth(0, 1);
    date.setHours(0, 0, 0, 0);
  }, function(date, step) {
    date.setFullYear(date.getFullYear() + step * k);
  });
};

var years = year.range;

var utcMinute = newInterval(function(date) {
  date.setUTCSeconds(0, 0);
}, function(date, step) {
  date.setTime(+date + step * durationMinute$1);
}, function(start, end) {
  return (end - start) / durationMinute$1;
}, function(date) {
  return date.getUTCMinutes();
});

var utcHour = newInterval(function(date) {
  date.setUTCMinutes(0, 0, 0);
}, function(date, step) {
  date.setTime(+date + step * durationHour$1);
}, function(start, end) {
  return (end - start) / durationHour$1;
}, function(date) {
  return date.getUTCHours();
});

var utcDay = newInterval(function(date) {
  date.setUTCHours(0, 0, 0, 0);
}, function(date, step) {
  date.setUTCDate(date.getUTCDate() + step);
}, function(start, end) {
  return (end - start) / durationDay$1;
}, function(date) {
  return date.getUTCDate() - 1;
});

function utcWeekday(i) {
  return newInterval(function(date) {
    date.setUTCDate(date.getUTCDate() - (date.getUTCDay() + 7 - i) % 7);
    date.setUTCHours(0, 0, 0, 0);
  }, function(date, step) {
    date.setUTCDate(date.getUTCDate() + step * 7);
  }, function(start, end) {
    return (end - start) / durationWeek$1;
  });
}

var utcSunday = utcWeekday(0);
var utcMonday = utcWeekday(1);
var utcTuesday = utcWeekday(2);
var utcWednesday = utcWeekday(3);
var utcThursday = utcWeekday(4);
var utcFriday = utcWeekday(5);
var utcSaturday = utcWeekday(6);

var utcMonth = newInterval(function(date) {
  date.setUTCDate(1);
  date.setUTCHours(0, 0, 0, 0);
}, function(date, step) {
  date.setUTCMonth(date.getUTCMonth() + step);
}, function(start, end) {
  return end.getUTCMonth() - start.getUTCMonth() + (end.getUTCFullYear() - start.getUTCFullYear()) * 12;
}, function(date) {
  return date.getUTCMonth();
});

var utcYear = newInterval(function(date) {
  date.setUTCMonth(0, 1);
  date.setUTCHours(0, 0, 0, 0);
}, function(date, step) {
  date.setUTCFullYear(date.getUTCFullYear() + step);
}, function(start, end) {
  return end.getUTCFullYear() - start.getUTCFullYear();
}, function(date) {
  return date.getUTCFullYear();
});

// An optimized implementation for this simple case.
utcYear.every = function(k) {
  return !isFinite(k = Math.floor(k)) || !(k > 0) ? null : newInterval(function(date) {
    date.setUTCFullYear(Math.floor(date.getUTCFullYear() / k) * k);
    date.setUTCMonth(0, 1);
    date.setUTCHours(0, 0, 0, 0);
  }, function(date, step) {
    date.setUTCFullYear(date.getUTCFullYear() + step * k);
  });
};

function localDate(d) {
  if (0 <= d.y && d.y < 100) {
    var date = new Date(-1, d.m, d.d, d.H, d.M, d.S, d.L);
    date.setFullYear(d.y);
    return date;
  }
  return new Date(d.y, d.m, d.d, d.H, d.M, d.S, d.L);
}

function utcDate(d) {
  if (0 <= d.y && d.y < 100) {
    var date = new Date(Date.UTC(-1, d.m, d.d, d.H, d.M, d.S, d.L));
    date.setUTCFullYear(d.y);
    return date;
  }
  return new Date(Date.UTC(d.y, d.m, d.d, d.H, d.M, d.S, d.L));
}

function newYear(y) {
  return {y: y, m: 0, d: 1, H: 0, M: 0, S: 0, L: 0};
}

function formatLocale$1(locale) {
  var locale_dateTime = locale.dateTime,
      locale_date = locale.date,
      locale_time = locale.time,
      locale_periods = locale.periods,
      locale_weekdays = locale.days,
      locale_shortWeekdays = locale.shortDays,
      locale_months = locale.months,
      locale_shortMonths = locale.shortMonths;

  var periodRe = formatRe(locale_periods),
      periodLookup = formatLookup(locale_periods),
      weekdayRe = formatRe(locale_weekdays),
      weekdayLookup = formatLookup(locale_weekdays),
      shortWeekdayRe = formatRe(locale_shortWeekdays),
      shortWeekdayLookup = formatLookup(locale_shortWeekdays),
      monthRe = formatRe(locale_months),
      monthLookup = formatLookup(locale_months),
      shortMonthRe = formatRe(locale_shortMonths),
      shortMonthLookup = formatLookup(locale_shortMonths);

  var formats = {
    "a": formatShortWeekday,
    "A": formatWeekday,
    "b": formatShortMonth,
    "B": formatMonth,
    "c": null,
    "d": formatDayOfMonth,
    "e": formatDayOfMonth,
    "H": formatHour24,
    "I": formatHour12,
    "j": formatDayOfYear,
    "L": formatMilliseconds,
    "m": formatMonthNumber,
    "M": formatMinutes,
    "p": formatPeriod,
    "S": formatSeconds,
    "U": formatWeekNumberSunday,
    "w": formatWeekdayNumber,
    "W": formatWeekNumberMonday,
    "x": null,
    "X": null,
    "y": formatYear,
    "Y": formatFullYear,
    "Z": formatZone,
    "%": formatLiteralPercent
  };

  var utcFormats = {
    "a": formatUTCShortWeekday,
    "A": formatUTCWeekday,
    "b": formatUTCShortMonth,
    "B": formatUTCMonth,
    "c": null,
    "d": formatUTCDayOfMonth,
    "e": formatUTCDayOfMonth,
    "H": formatUTCHour24,
    "I": formatUTCHour12,
    "j": formatUTCDayOfYear,
    "L": formatUTCMilliseconds,
    "m": formatUTCMonthNumber,
    "M": formatUTCMinutes,
    "p": formatUTCPeriod,
    "S": formatUTCSeconds,
    "U": formatUTCWeekNumberSunday,
    "w": formatUTCWeekdayNumber,
    "W": formatUTCWeekNumberMonday,
    "x": null,
    "X": null,
    "y": formatUTCYear,
    "Y": formatUTCFullYear,
    "Z": formatUTCZone,
    "%": formatLiteralPercent
  };

  var parses = {
    "a": parseShortWeekday,
    "A": parseWeekday,
    "b": parseShortMonth,
    "B": parseMonth,
    "c": parseLocaleDateTime,
    "d": parseDayOfMonth,
    "e": parseDayOfMonth,
    "H": parseHour24,
    "I": parseHour24,
    "j": parseDayOfYear,
    "L": parseMilliseconds,
    "m": parseMonthNumber,
    "M": parseMinutes,
    "p": parsePeriod,
    "S": parseSeconds,
    "U": parseWeekNumberSunday,
    "w": parseWeekdayNumber,
    "W": parseWeekNumberMonday,
    "x": parseLocaleDate,
    "X": parseLocaleTime,
    "y": parseYear,
    "Y": parseFullYear,
    "Z": parseZone,
    "%": parseLiteralPercent
  };

  // These recursive directive definitions must be deferred.
  formats.x = newFormat(locale_date, formats);
  formats.X = newFormat(locale_time, formats);
  formats.c = newFormat(locale_dateTime, formats);
  utcFormats.x = newFormat(locale_date, utcFormats);
  utcFormats.X = newFormat(locale_time, utcFormats);
  utcFormats.c = newFormat(locale_dateTime, utcFormats);

  function newFormat(specifier, formats) {
    return function(date) {
      var string = [],
          i = -1,
          j = 0,
          n = specifier.length,
          c,
          pad,
          format;

      if (!(date instanceof Date)) date = new Date(+date);

      while (++i < n) {
        if (specifier.charCodeAt(i) === 37) {
          string.push(specifier.slice(j, i));
          if ((pad = pads[c = specifier.charAt(++i)]) != null) c = specifier.charAt(++i);
          else pad = c === "e" ? " " : "0";
          if (format = formats[c]) c = format(date, pad);
          string.push(c);
          j = i + 1;
        }
      }

      string.push(specifier.slice(j, i));
      return string.join("");
    };
  }

  function newParse(specifier, newDate) {
    return function(string) {
      var d = newYear(1900),
          i = parseSpecifier(d, specifier, string += "", 0);
      if (i != string.length) return null;

      // The am-pm flag is 0 for AM, and 1 for PM.
      if ("p" in d) d.H = d.H % 12 + d.p * 12;

      // Convert day-of-week and week-of-year to day-of-year.
      if ("W" in d || "U" in d) {
        if (!("w" in d)) d.w = "W" in d ? 1 : 0;
        var day$$1 = "Z" in d ? utcDate(newYear(d.y)).getUTCDay() : newDate(newYear(d.y)).getDay();
        d.m = 0;
        d.d = "W" in d ? (d.w + 6) % 7 + d.W * 7 - (day$$1 + 5) % 7 : d.w + d.U * 7 - (day$$1 + 6) % 7;
      }

      // If a time zone is specified, all fields are interpreted as UTC and then
      // offset according to the specified time zone.
      if ("Z" in d) {
        d.H += d.Z / 100 | 0;
        d.M += d.Z % 100;
        return utcDate(d);
      }

      // Otherwise, all fields are in local time.
      return newDate(d);
    };
  }

  function parseSpecifier(d, specifier, string, j) {
    var i = 0,
        n = specifier.length,
        m = string.length,
        c,
        parse;

    while (i < n) {
      if (j >= m) return -1;
      c = specifier.charCodeAt(i++);
      if (c === 37) {
        c = specifier.charAt(i++);
        parse = parses[c in pads ? specifier.charAt(i++) : c];
        if (!parse || ((j = parse(d, string, j)) < 0)) return -1;
      } else if (c != string.charCodeAt(j++)) {
        return -1;
      }
    }

    return j;
  }

  function parsePeriod(d, string, i) {
    var n = periodRe.exec(string.slice(i));
    return n ? (d.p = periodLookup[n[0].toLowerCase()], i + n[0].length) : -1;
  }

  function parseShortWeekday(d, string, i) {
    var n = shortWeekdayRe.exec(string.slice(i));
    return n ? (d.w = shortWeekdayLookup[n[0].toLowerCase()], i + n[0].length) : -1;
  }

  function parseWeekday(d, string, i) {
    var n = weekdayRe.exec(string.slice(i));
    return n ? (d.w = weekdayLookup[n[0].toLowerCase()], i + n[0].length) : -1;
  }

  function parseShortMonth(d, string, i) {
    var n = shortMonthRe.exec(string.slice(i));
    return n ? (d.m = shortMonthLookup[n[0].toLowerCase()], i + n[0].length) : -1;
  }

  function parseMonth(d, string, i) {
    var n = monthRe.exec(string.slice(i));
    return n ? (d.m = monthLookup[n[0].toLowerCase()], i + n[0].length) : -1;
  }

  function parseLocaleDateTime(d, string, i) {
    return parseSpecifier(d, locale_dateTime, string, i);
  }

  function parseLocaleDate(d, string, i) {
    return parseSpecifier(d, locale_date, string, i);
  }

  function parseLocaleTime(d, string, i) {
    return parseSpecifier(d, locale_time, string, i);
  }

  function formatShortWeekday(d) {
    return locale_shortWeekdays[d.getDay()];
  }

  function formatWeekday(d) {
    return locale_weekdays[d.getDay()];
  }

  function formatShortMonth(d) {
    return locale_shortMonths[d.getMonth()];
  }

  function formatMonth(d) {
    return locale_months[d.getMonth()];
  }

  function formatPeriod(d) {
    return locale_periods[+(d.getHours() >= 12)];
  }

  function formatUTCShortWeekday(d) {
    return locale_shortWeekdays[d.getUTCDay()];
  }

  function formatUTCWeekday(d) {
    return locale_weekdays[d.getUTCDay()];
  }

  function formatUTCShortMonth(d) {
    return locale_shortMonths[d.getUTCMonth()];
  }

  function formatUTCMonth(d) {
    return locale_months[d.getUTCMonth()];
  }

  function formatUTCPeriod(d) {
    return locale_periods[+(d.getUTCHours() >= 12)];
  }

  return {
    format: function(specifier) {
      var f = newFormat(specifier += "", formats);
      f.toString = function() { return specifier; };
      return f;
    },
    parse: function(specifier) {
      var p = newParse(specifier += "", localDate);
      p.toString = function() { return specifier; };
      return p;
    },
    utcFormat: function(specifier) {
      var f = newFormat(specifier += "", utcFormats);
      f.toString = function() { return specifier; };
      return f;
    },
    utcParse: function(specifier) {
      var p = newParse(specifier, utcDate);
      p.toString = function() { return specifier; };
      return p;
    }
  };
}

var pads = {"-": "", "_": " ", "0": "0"};
var numberRe = /^\s*\d+/;
var percentRe = /^%/;
var requoteRe = /[\\\^\$\*\+\?\|\[\]\(\)\.\{\}]/g;

function pad(value, fill, width) {
  var sign = value < 0 ? "-" : "",
      string = (sign ? -value : value) + "",
      length = string.length;
  return sign + (length < width ? new Array(width - length + 1).join(fill) + string : string);
}

function requote(s) {
  return s.replace(requoteRe, "\\$&");
}

function formatRe(names) {
  return new RegExp("^(?:" + names.map(requote).join("|") + ")", "i");
}

function formatLookup(names) {
  var map = {}, i = -1, n = names.length;
  while (++i < n) map[names[i].toLowerCase()] = i;
  return map;
}

function parseWeekdayNumber(d, string, i) {
  var n = numberRe.exec(string.slice(i, i + 1));
  return n ? (d.w = +n[0], i + n[0].length) : -1;
}

function parseWeekNumberSunday(d, string, i) {
  var n = numberRe.exec(string.slice(i));
  return n ? (d.U = +n[0], i + n[0].length) : -1;
}

function parseWeekNumberMonday(d, string, i) {
  var n = numberRe.exec(string.slice(i));
  return n ? (d.W = +n[0], i + n[0].length) : -1;
}

function parseFullYear(d, string, i) {
  var n = numberRe.exec(string.slice(i, i + 4));
  return n ? (d.y = +n[0], i + n[0].length) : -1;
}

function parseYear(d, string, i) {
  var n = numberRe.exec(string.slice(i, i + 2));
  return n ? (d.y = +n[0] + (+n[0] > 68 ? 1900 : 2000), i + n[0].length) : -1;
}

function parseZone(d, string, i) {
  var n = /^(Z)|([+-]\d\d)(?:\:?(\d\d))?/.exec(string.slice(i, i + 6));
  return n ? (d.Z = n[1] ? 0 : -(n[2] + (n[3] || "00")), i + n[0].length) : -1;
}

function parseMonthNumber(d, string, i) {
  var n = numberRe.exec(string.slice(i, i + 2));
  return n ? (d.m = n[0] - 1, i + n[0].length) : -1;
}

function parseDayOfMonth(d, string, i) {
  var n = numberRe.exec(string.slice(i, i + 2));
  return n ? (d.d = +n[0], i + n[0].length) : -1;
}

function parseDayOfYear(d, string, i) {
  var n = numberRe.exec(string.slice(i, i + 3));
  return n ? (d.m = 0, d.d = +n[0], i + n[0].length) : -1;
}

function parseHour24(d, string, i) {
  var n = numberRe.exec(string.slice(i, i + 2));
  return n ? (d.H = +n[0], i + n[0].length) : -1;
}

function parseMinutes(d, string, i) {
  var n = numberRe.exec(string.slice(i, i + 2));
  return n ? (d.M = +n[0], i + n[0].length) : -1;
}

function parseSeconds(d, string, i) {
  var n = numberRe.exec(string.slice(i, i + 2));
  return n ? (d.S = +n[0], i + n[0].length) : -1;
}

function parseMilliseconds(d, string, i) {
  var n = numberRe.exec(string.slice(i, i + 3));
  return n ? (d.L = +n[0], i + n[0].length) : -1;
}

function parseLiteralPercent(d, string, i) {
  var n = percentRe.exec(string.slice(i, i + 1));
  return n ? i + n[0].length : -1;
}

function formatDayOfMonth(d, p) {
  return pad(d.getDate(), p, 2);
}

function formatHour24(d, p) {
  return pad(d.getHours(), p, 2);
}

function formatHour12(d, p) {
  return pad(d.getHours() % 12 || 12, p, 2);
}

function formatDayOfYear(d, p) {
  return pad(1 + day.count(year(d), d), p, 3);
}

function formatMilliseconds(d, p) {
  return pad(d.getMilliseconds(), p, 3);
}

function formatMonthNumber(d, p) {
  return pad(d.getMonth() + 1, p, 2);
}

function formatMinutes(d, p) {
  return pad(d.getMinutes(), p, 2);
}

function formatSeconds(d, p) {
  return pad(d.getSeconds(), p, 2);
}

function formatWeekNumberSunday(d, p) {
  return pad(sunday.count(year(d), d), p, 2);
}

function formatWeekdayNumber(d) {
  return d.getDay();
}

function formatWeekNumberMonday(d, p) {
  return pad(monday.count(year(d), d), p, 2);
}

function formatYear(d, p) {
  return pad(d.getFullYear() % 100, p, 2);
}

function formatFullYear(d, p) {
  return pad(d.getFullYear() % 10000, p, 4);
}

function formatZone(d) {
  var z = d.getTimezoneOffset();
  return (z > 0 ? "-" : (z *= -1, "+"))
      + pad(z / 60 | 0, "0", 2)
      + pad(z % 60, "0", 2);
}

function formatUTCDayOfMonth(d, p) {
  return pad(d.getUTCDate(), p, 2);
}

function formatUTCHour24(d, p) {
  return pad(d.getUTCHours(), p, 2);
}

function formatUTCHour12(d, p) {
  return pad(d.getUTCHours() % 12 || 12, p, 2);
}

function formatUTCDayOfYear(d, p) {
  return pad(1 + utcDay.count(utcYear(d), d), p, 3);
}

function formatUTCMilliseconds(d, p) {
  return pad(d.getUTCMilliseconds(), p, 3);
}

function formatUTCMonthNumber(d, p) {
  return pad(d.getUTCMonth() + 1, p, 2);
}

function formatUTCMinutes(d, p) {
  return pad(d.getUTCMinutes(), p, 2);
}

function formatUTCSeconds(d, p) {
  return pad(d.getUTCSeconds(), p, 2);
}

function formatUTCWeekNumberSunday(d, p) {
  return pad(utcSunday.count(utcYear(d), d), p, 2);
}

function formatUTCWeekdayNumber(d) {
  return d.getUTCDay();
}

function formatUTCWeekNumberMonday(d, p) {
  return pad(utcMonday.count(utcYear(d), d), p, 2);
}

function formatUTCYear(d, p) {
  return pad(d.getUTCFullYear() % 100, p, 2);
}

function formatUTCFullYear(d, p) {
  return pad(d.getUTCFullYear() % 10000, p, 4);
}

function formatUTCZone() {
  return "+0000";
}

function formatLiteralPercent() {
  return "%";
}

var locale$2;
var timeFormat;
var timeParse;
var utcFormat;
var utcParse;

defaultLocale$1({
  dateTime: "%x, %X",
  date: "%-m/%-d/%Y",
  time: "%-I:%M:%S %p",
  periods: ["AM", "PM"],
  days: ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"],
  shortDays: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
  months: ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
  shortMonths: ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
});

function defaultLocale$1(definition) {
  locale$2 = formatLocale$1(definition);
  timeFormat = locale$2.format;
  timeParse = locale$2.parse;
  utcFormat = locale$2.utcFormat;
  utcParse = locale$2.utcParse;
  return locale$2;
}

var isoSpecifier = "%Y-%m-%dT%H:%M:%S.%LZ";

function formatIsoNative(date) {
  return date.toISOString();
}

var formatIso = Date.prototype.toISOString
    ? formatIsoNative
    : utcFormat(isoSpecifier);

function parseIsoNative(string) {
  var date = new Date(string);
  return isNaN(date) ? null : date;
}

var parseIso = +new Date("2000-01-01T00:00:00.000Z")
    ? parseIsoNative
    : utcParse(isoSpecifier);

var durationSecond = 1000;
var durationMinute = durationSecond * 60;
var durationHour = durationMinute * 60;
var durationDay = durationHour * 24;
var durationWeek = durationDay * 7;
var durationMonth = durationDay * 30;
var durationYear = durationDay * 365;

function date$1(t) {
  return new Date(t);
}

function number$2(t) {
  return t instanceof Date ? +t : +new Date(+t);
}

function calendar(year$$1, month$$1, week, day$$1, hour$$1, minute$$1, second$$1, millisecond$$1, format) {
  var scale = continuous(deinterpolateLinear, reinterpolate),
      invert = scale.invert,
      domain = scale.domain;

  var formatMillisecond = format(".%L"),
      formatSecond = format(":%S"),
      formatMinute = format("%I:%M"),
      formatHour = format("%I %p"),
      formatDay = format("%a %d"),
      formatWeek = format("%b %d"),
      formatMonth = format("%B"),
      formatYear = format("%Y");

  var tickIntervals = [
    [second$$1,  1,      durationSecond],
    [second$$1,  5,  5 * durationSecond],
    [second$$1, 15, 15 * durationSecond],
    [second$$1, 30, 30 * durationSecond],
    [minute$$1,  1,      durationMinute],
    [minute$$1,  5,  5 * durationMinute],
    [minute$$1, 15, 15 * durationMinute],
    [minute$$1, 30, 30 * durationMinute],
    [  hour$$1,  1,      durationHour  ],
    [  hour$$1,  3,  3 * durationHour  ],
    [  hour$$1,  6,  6 * durationHour  ],
    [  hour$$1, 12, 12 * durationHour  ],
    [   day$$1,  1,      durationDay   ],
    [   day$$1,  2,  2 * durationDay   ],
    [  week,  1,      durationWeek  ],
    [ month$$1,  1,      durationMonth ],
    [ month$$1,  3,  3 * durationMonth ],
    [  year$$1,  1,      durationYear  ]
  ];

  function tickFormat(date) {
    return (second$$1(date) < date ? formatMillisecond
        : minute$$1(date) < date ? formatSecond
        : hour$$1(date) < date ? formatMinute
        : day$$1(date) < date ? formatHour
        : month$$1(date) < date ? (week(date) < date ? formatDay : formatWeek)
        : year$$1(date) < date ? formatMonth
        : formatYear)(date);
  }

  function tickInterval(interval, start, stop, step) {
    if (interval == null) interval = 10;

    // If a desired tick count is specified, pick a reasonable tick interval
    // based on the extent of the domain and a rough estimate of tick size.
    // Otherwise, assume interval is already a time interval and use it.
    if (typeof interval === "number") {
      var target = Math.abs(stop - start) / interval,
          i = bisector(function(i) { return i[2]; }).right(tickIntervals, target);
      if (i === tickIntervals.length) {
        step = tickStep(start / durationYear, stop / durationYear, interval);
        interval = year$$1;
      } else if (i) {
        i = tickIntervals[target / tickIntervals[i - 1][2] < tickIntervals[i][2] / target ? i - 1 : i];
        step = i[1];
        interval = i[0];
      } else {
        step = tickStep(start, stop, interval);
        interval = millisecond$$1;
      }
    }

    return step == null ? interval : interval.every(step);
  }

  scale.invert = function(y) {
    return new Date(invert(y));
  };

  scale.domain = function(_) {
    return arguments.length ? domain(map$3.call(_, number$2)) : domain().map(date$1);
  };

  scale.ticks = function(interval, step) {
    var d = domain(),
        t0 = d[0],
        t1 = d[d.length - 1],
        r = t1 < t0,
        t;
    if (r) t = t0, t0 = t1, t1 = t;
    t = tickInterval(interval, t0, t1, step);
    t = t ? t.range(t0, t1 + 1) : []; // inclusive stop
    return r ? t.reverse() : t;
  };

  scale.tickFormat = function(count, specifier) {
    return specifier == null ? tickFormat : format(specifier);
  };

  scale.nice = function(interval, step) {
    var d = domain();
    return (interval = tickInterval(interval, d[0], d[d.length - 1], step))
        ? domain(nice(d, interval))
        : scale;
  };

  scale.copy = function() {
    return copy(scale, calendar(year$$1, month$$1, week, day$$1, hour$$1, minute$$1, second$$1, millisecond$$1, format));
  };

  return scale;
}

var colors = function(s) {
  return s.match(/.{6}/g).map(function(x) {
    return "#" + x;
  });
};

colors("1f77b4ff7f0e2ca02cd627289467bd8c564be377c27f7f7fbcbd2217becf");

colors("393b795254a36b6ecf9c9ede6379398ca252b5cf6bcedb9c8c6d31bd9e39e7ba52e7cb94843c39ad494ad6616be7969c7b4173a55194ce6dbdde9ed6");

colors("3182bd6baed69ecae1c6dbefe6550dfd8d3cfdae6bfdd0a231a35474c476a1d99bc7e9c0756bb19e9ac8bcbddcdadaeb636363969696bdbdbdd9d9d9");

colors("1f77b4aec7e8ff7f0effbb782ca02c98df8ad62728ff98969467bdc5b0d58c564bc49c94e377c2f7b6d27f7f7fc7c7c7bcbd22dbdb8d17becf9edae5");

cubehelixLong(cubehelix(300, 0.5, 0.0), cubehelix(-240, 0.5, 1.0));

var warm = cubehelixLong(cubehelix(-100, 0.75, 0.35), cubehelix(80, 1.50, 0.8));

var cool = cubehelixLong(cubehelix(260, 0.75, 0.35), cubehelix(80, 1.50, 0.8));

var rainbow = cubehelix();

function sequential(interpolator) {
  var x0 = 0,
      x1 = 1,
      clamp = false;

  function scale(x) {
    var t = (x - x0) / (x1 - x0);
    return interpolator(clamp ? Math.max(0, Math.min(1, t)) : t);
  }

  scale.domain = function(_) {
    return arguments.length ? (x0 = +_[0], x1 = +_[1], scale) : [x0, x1];
  };

  scale.clamp = function(_) {
    return arguments.length ? (clamp = !!_, scale) : clamp;
  };

  scale.interpolator = function(_) {
    return arguments.length ? (interpolator = _, scale) : interpolator;
  };

  scale.copy = function() {
    return sequential(interpolator).domain([x0, x1]).clamp(clamp);
  };

  return linearish(scale);
}

var xhtml = "http://www.w3.org/1999/xhtml";

var namespaces = {
  svg: "http://www.w3.org/2000/svg",
  xhtml: xhtml,
  xlink: "http://www.w3.org/1999/xlink",
  xml: "http://www.w3.org/XML/1998/namespace",
  xmlns: "http://www.w3.org/2000/xmlns/"
};

var namespace = function(name) {
  var prefix = name += "", i = prefix.indexOf(":");
  if (i >= 0 && (prefix = name.slice(0, i)) !== "xmlns") name = name.slice(i + 1);
  return namespaces.hasOwnProperty(prefix) ? {space: namespaces[prefix], local: name} : name;
};

function creatorInherit(name) {
  return function() {
    var document = this.ownerDocument,
        uri = this.namespaceURI;
    return uri === xhtml && document.documentElement.namespaceURI === xhtml
        ? document.createElement(name)
        : document.createElementNS(uri, name);
  };
}

function creatorFixed(fullname) {
  return function() {
    return this.ownerDocument.createElementNS(fullname.space, fullname.local);
  };
}

var creator = function(name) {
  var fullname = namespace(name);
  return (fullname.local
      ? creatorFixed
      : creatorInherit)(fullname);
};

var nextId = 0;

var matcher = function(selector) {
  return function() {
    return this.matches(selector);
  };
};

if (typeof document !== "undefined") {
  var element = document.documentElement;
  if (!element.matches) {
    var vendorMatches = element.webkitMatchesSelector
        || element.msMatchesSelector
        || element.mozMatchesSelector
        || element.oMatchesSelector;
    matcher = function(selector) {
      return function() {
        return vendorMatches.call(this, selector);
      };
    };
  }
}

var matcher$1 = matcher;

var filterEvents = {};

var event = null;

if (typeof document !== "undefined") {
  var element$1 = document.documentElement;
  if (!("onmouseenter" in element$1)) {
    filterEvents = {mouseenter: "mouseover", mouseleave: "mouseout"};
  }
}

function filterContextListener(listener, index, group) {
  listener = contextListener(listener, index, group);
  return function(event) {
    var related = event.relatedTarget;
    if (!related || (related !== this && !(related.compareDocumentPosition(this) & 8))) {
      listener.call(this, event);
    }
  };
}

function contextListener(listener, index, group) {
  return function(event1) {
    var event0 = event; // Events can be reentrant (e.g., focus).
    event = event1;
    try {
      listener.call(this, this.__data__, index, group);
    } finally {
      event = event0;
    }
  };
}

function parseTypenames$1(typenames) {
  return typenames.trim().split(/^|\s+/).map(function(t) {
    var name = "", i = t.indexOf(".");
    if (i >= 0) name = t.slice(i + 1), t = t.slice(0, i);
    return {type: t, name: name};
  });
}

function onRemove(typename) {
  return function() {
    var on = this.__on;
    if (!on) return;
    for (var j = 0, i = -1, m = on.length, o; j < m; ++j) {
      if (o = on[j], (!typename.type || o.type === typename.type) && o.name === typename.name) {
        this.removeEventListener(o.type, o.listener, o.capture);
      } else {
        on[++i] = o;
      }
    }
    if (++i) on.length = i;
    else delete this.__on;
  };
}

function onAdd(typename, value, capture) {
  var wrap = filterEvents.hasOwnProperty(typename.type) ? filterContextListener : contextListener;
  return function(d, i, group) {
    var on = this.__on, o, listener = wrap(value, i, group);
    if (on) for (var j = 0, m = on.length; j < m; ++j) {
      if ((o = on[j]).type === typename.type && o.name === typename.name) {
        this.removeEventListener(o.type, o.listener, o.capture);
        this.addEventListener(o.type, o.listener = listener, o.capture = capture);
        o.value = value;
        return;
      }
    }
    this.addEventListener(typename.type, listener, capture);
    o = {type: typename.type, name: typename.name, value: value, listener: listener, capture: capture};
    if (!on) this.__on = [o];
    else on.push(o);
  };
}

var selection_on = function(typename, value, capture) {
  var typenames = parseTypenames$1(typename + ""), i, n = typenames.length, t;

  if (arguments.length < 2) {
    var on = this.node().__on;
    if (on) for (var j = 0, m = on.length, o; j < m; ++j) {
      for (i = 0, o = on[j]; i < n; ++i) {
        if ((t = typenames[i]).type === o.type && t.name === o.name) {
          return o.value;
        }
      }
    }
    return;
  }

  on = value ? onAdd : onRemove;
  if (capture == null) capture = false;
  for (i = 0; i < n; ++i) this.each(on(typenames[i], value, capture));
  return this;
};

var sourceEvent = function() {
  var current = event, source;
  while (source = current.sourceEvent) current = source;
  return current;
};

var point$1 = function(node, event) {
  var svg = node.ownerSVGElement || node;

  if (svg.createSVGPoint) {
    var point = svg.createSVGPoint();
    point.x = event.clientX, point.y = event.clientY;
    point = point.matrixTransform(node.getScreenCTM().inverse());
    return [point.x, point.y];
  }

  var rect = node.getBoundingClientRect();
  return [event.clientX - rect.left - node.clientLeft, event.clientY - rect.top - node.clientTop];
};

function none() {}

var selector = function(selector) {
  return selector == null ? none : function() {
    return this.querySelector(selector);
  };
};

var selection_select = function(select) {
  if (typeof select !== "function") select = selector(select);

  for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
    for (var group = groups[j], n = group.length, subgroup = subgroups[j] = new Array(n), node, subnode, i = 0; i < n; ++i) {
      if ((node = group[i]) && (subnode = select.call(node, node.__data__, i, group))) {
        if ("__data__" in node) subnode.__data__ = node.__data__;
        subgroup[i] = subnode;
      }
    }
  }

  return new Selection(subgroups, this._parents);
};

function empty() {
  return [];
}

var selectorAll = function(selector) {
  return selector == null ? empty : function() {
    return this.querySelectorAll(selector);
  };
};

var selection_selectAll = function(select) {
  if (typeof select !== "function") select = selectorAll(select);

  for (var groups = this._groups, m = groups.length, subgroups = [], parents = [], j = 0; j < m; ++j) {
    for (var group = groups[j], n = group.length, node, i = 0; i < n; ++i) {
      if (node = group[i]) {
        subgroups.push(select.call(node, node.__data__, i, group));
        parents.push(node);
      }
    }
  }

  return new Selection(subgroups, parents);
};

var selection_filter = function(match) {
  if (typeof match !== "function") match = matcher$1(match);

  for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
    for (var group = groups[j], n = group.length, subgroup = subgroups[j] = [], node, i = 0; i < n; ++i) {
      if ((node = group[i]) && match.call(node, node.__data__, i, group)) {
        subgroup.push(node);
      }
    }
  }

  return new Selection(subgroups, this._parents);
};

var sparse = function(update) {
  return new Array(update.length);
};

var selection_enter = function() {
  return new Selection(this._enter || this._groups.map(sparse), this._parents);
};

function EnterNode(parent, datum) {
  this.ownerDocument = parent.ownerDocument;
  this.namespaceURI = parent.namespaceURI;
  this._next = null;
  this._parent = parent;
  this.__data__ = datum;
}

EnterNode.prototype = {
  constructor: EnterNode,
  appendChild: function(child) { return this._parent.insertBefore(child, this._next); },
  insertBefore: function(child, next) { return this._parent.insertBefore(child, next); },
  querySelector: function(selector) { return this._parent.querySelector(selector); },
  querySelectorAll: function(selector) { return this._parent.querySelectorAll(selector); }
};

var constant$3 = function(x) {
  return function() {
    return x;
  };
};

var keyPrefix = "$"; // Protect against keys like “__proto__”.

function bindIndex(parent, group, enter, update, exit, data) {
  var i = 0,
      node,
      groupLength = group.length,
      dataLength = data.length;

  // Put any non-null nodes that fit into update.
  // Put any null nodes into enter.
  // Put any remaining data into enter.
  for (; i < dataLength; ++i) {
    if (node = group[i]) {
      node.__data__ = data[i];
      update[i] = node;
    } else {
      enter[i] = new EnterNode(parent, data[i]);
    }
  }

  // Put any non-null nodes that don’t fit into exit.
  for (; i < groupLength; ++i) {
    if (node = group[i]) {
      exit[i] = node;
    }
  }
}

function bindKey(parent, group, enter, update, exit, data, key) {
  var i,
      node,
      nodeByKeyValue = {},
      groupLength = group.length,
      dataLength = data.length,
      keyValues = new Array(groupLength),
      keyValue;

  // Compute the key for each node.
  // If multiple nodes have the same key, the duplicates are added to exit.
  for (i = 0; i < groupLength; ++i) {
    if (node = group[i]) {
      keyValues[i] = keyValue = keyPrefix + key.call(node, node.__data__, i, group);
      if (keyValue in nodeByKeyValue) {
        exit[i] = node;
      } else {
        nodeByKeyValue[keyValue] = node;
      }
    }
  }

  // Compute the key for each datum.
  // If there a node associated with this key, join and add it to update.
  // If there is not (or the key is a duplicate), add it to enter.
  for (i = 0; i < dataLength; ++i) {
    keyValue = keyPrefix + key.call(parent, data[i], i, data);
    if (node = nodeByKeyValue[keyValue]) {
      update[i] = node;
      node.__data__ = data[i];
      nodeByKeyValue[keyValue] = null;
    } else {
      enter[i] = new EnterNode(parent, data[i]);
    }
  }

  // Add any remaining nodes that were not bound to data to exit.
  for (i = 0; i < groupLength; ++i) {
    if ((node = group[i]) && (nodeByKeyValue[keyValues[i]] === node)) {
      exit[i] = node;
    }
  }
}

var selection_data = function(value, key) {
  if (!value) {
    data = new Array(this.size()), j = -1;
    this.each(function(d) { data[++j] = d; });
    return data;
  }

  var bind = key ? bindKey : bindIndex,
      parents = this._parents,
      groups = this._groups;

  if (typeof value !== "function") value = constant$3(value);

  for (var m = groups.length, update = new Array(m), enter = new Array(m), exit = new Array(m), j = 0; j < m; ++j) {
    var parent = parents[j],
        group = groups[j],
        groupLength = group.length,
        data = value.call(parent, parent && parent.__data__, j, parents),
        dataLength = data.length,
        enterGroup = enter[j] = new Array(dataLength),
        updateGroup = update[j] = new Array(dataLength),
        exitGroup = exit[j] = new Array(groupLength);

    bind(parent, group, enterGroup, updateGroup, exitGroup, data, key);

    // Now connect the enter nodes to their following update node, such that
    // appendChild can insert the materialized enter node before this node,
    // rather than at the end of the parent node.
    for (var i0 = 0, i1 = 0, previous, next; i0 < dataLength; ++i0) {
      if (previous = enterGroup[i0]) {
        if (i0 >= i1) i1 = i0 + 1;
        while (!(next = updateGroup[i1]) && ++i1 < dataLength);
        previous._next = next || null;
      }
    }
  }

  update = new Selection(update, parents);
  update._enter = enter;
  update._exit = exit;
  return update;
};

var selection_exit = function() {
  return new Selection(this._exit || this._groups.map(sparse), this._parents);
};

var selection_merge = function(selection) {

  for (var groups0 = this._groups, groups1 = selection._groups, m0 = groups0.length, m1 = groups1.length, m = Math.min(m0, m1), merges = new Array(m0), j = 0; j < m; ++j) {
    for (var group0 = groups0[j], group1 = groups1[j], n = group0.length, merge = merges[j] = new Array(n), node, i = 0; i < n; ++i) {
      if (node = group0[i] || group1[i]) {
        merge[i] = node;
      }
    }
  }

  for (; j < m0; ++j) {
    merges[j] = groups0[j];
  }

  return new Selection(merges, this._parents);
};

var selection_order = function() {

  for (var groups = this._groups, j = -1, m = groups.length; ++j < m;) {
    for (var group = groups[j], i = group.length - 1, next = group[i], node; --i >= 0;) {
      if (node = group[i]) {
        if (next && next !== node.nextSibling) next.parentNode.insertBefore(node, next);
        next = node;
      }
    }
  }

  return this;
};

var selection_sort = function(compare) {
  if (!compare) compare = ascending$1;

  function compareNode(a, b) {
    return a && b ? compare(a.__data__, b.__data__) : !a - !b;
  }

  for (var groups = this._groups, m = groups.length, sortgroups = new Array(m), j = 0; j < m; ++j) {
    for (var group = groups[j], n = group.length, sortgroup = sortgroups[j] = new Array(n), node, i = 0; i < n; ++i) {
      if (node = group[i]) {
        sortgroup[i] = node;
      }
    }
    sortgroup.sort(compareNode);
  }

  return new Selection(sortgroups, this._parents).order();
};

function ascending$1(a, b) {
  return a < b ? -1 : a > b ? 1 : a >= b ? 0 : NaN;
}

var selection_call = function() {
  var callback = arguments[0];
  arguments[0] = this;
  callback.apply(null, arguments);
  return this;
};

var selection_nodes = function() {
  var nodes = new Array(this.size()), i = -1;
  this.each(function() { nodes[++i] = this; });
  return nodes;
};

var selection_node = function() {

  for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
    for (var group = groups[j], i = 0, n = group.length; i < n; ++i) {
      var node = group[i];
      if (node) return node;
    }
  }

  return null;
};

var selection_size = function() {
  var size = 0;
  this.each(function() { ++size; });
  return size;
};

var selection_empty = function() {
  return !this.node();
};

var selection_each = function(callback) {

  for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
    for (var group = groups[j], i = 0, n = group.length, node; i < n; ++i) {
      if (node = group[i]) callback.call(node, node.__data__, i, group);
    }
  }

  return this;
};

function attrRemove(name) {
  return function() {
    this.removeAttribute(name);
  };
}

function attrRemoveNS(fullname) {
  return function() {
    this.removeAttributeNS(fullname.space, fullname.local);
  };
}

function attrConstant(name, value) {
  return function() {
    this.setAttribute(name, value);
  };
}

function attrConstantNS(fullname, value) {
  return function() {
    this.setAttributeNS(fullname.space, fullname.local, value);
  };
}

function attrFunction(name, value) {
  return function() {
    var v = value.apply(this, arguments);
    if (v == null) this.removeAttribute(name);
    else this.setAttribute(name, v);
  };
}

function attrFunctionNS(fullname, value) {
  return function() {
    var v = value.apply(this, arguments);
    if (v == null) this.removeAttributeNS(fullname.space, fullname.local);
    else this.setAttributeNS(fullname.space, fullname.local, v);
  };
}

var selection_attr = function(name, value) {
  var fullname = namespace(name);

  if (arguments.length < 2) {
    var node = this.node();
    return fullname.local
        ? node.getAttributeNS(fullname.space, fullname.local)
        : node.getAttribute(fullname);
  }

  return this.each((value == null
      ? (fullname.local ? attrRemoveNS : attrRemove) : (typeof value === "function"
      ? (fullname.local ? attrFunctionNS : attrFunction)
      : (fullname.local ? attrConstantNS : attrConstant)))(fullname, value));
};

var defaultView = function(node) {
  return (node.ownerDocument && node.ownerDocument.defaultView) // node is a Node
      || (node.document && node) // node is a Window
      || node.defaultView; // node is a Document
};

function styleRemove(name) {
  return function() {
    this.style.removeProperty(name);
  };
}

function styleConstant(name, value, priority) {
  return function() {
    this.style.setProperty(name, value, priority);
  };
}

function styleFunction(name, value, priority) {
  return function() {
    var v = value.apply(this, arguments);
    if (v == null) this.style.removeProperty(name);
    else this.style.setProperty(name, v, priority);
  };
}

var selection_style = function(name, value, priority) {
  var node;
  return arguments.length > 1
      ? this.each((value == null
            ? styleRemove : typeof value === "function"
            ? styleFunction
            : styleConstant)(name, value, priority == null ? "" : priority))
      : defaultView(node = this.node())
          .getComputedStyle(node, null)
          .getPropertyValue(name);
};

function propertyRemove(name) {
  return function() {
    delete this[name];
  };
}

function propertyConstant(name, value) {
  return function() {
    this[name] = value;
  };
}

function propertyFunction(name, value) {
  return function() {
    var v = value.apply(this, arguments);
    if (v == null) delete this[name];
    else this[name] = v;
  };
}

var selection_property = function(name, value) {
  return arguments.length > 1
      ? this.each((value == null
          ? propertyRemove : typeof value === "function"
          ? propertyFunction
          : propertyConstant)(name, value))
      : this.node()[name];
};

function classArray(string) {
  return string.trim().split(/^|\s+/);
}

function classList(node) {
  return node.classList || new ClassList(node);
}

function ClassList(node) {
  this._node = node;
  this._names = classArray(node.getAttribute("class") || "");
}

ClassList.prototype = {
  add: function(name) {
    var i = this._names.indexOf(name);
    if (i < 0) {
      this._names.push(name);
      this._node.setAttribute("class", this._names.join(" "));
    }
  },
  remove: function(name) {
    var i = this._names.indexOf(name);
    if (i >= 0) {
      this._names.splice(i, 1);
      this._node.setAttribute("class", this._names.join(" "));
    }
  },
  contains: function(name) {
    return this._names.indexOf(name) >= 0;
  }
};

function classedAdd(node, names) {
  var list = classList(node), i = -1, n = names.length;
  while (++i < n) list.add(names[i]);
}

function classedRemove(node, names) {
  var list = classList(node), i = -1, n = names.length;
  while (++i < n) list.remove(names[i]);
}

function classedTrue(names) {
  return function() {
    classedAdd(this, names);
  };
}

function classedFalse(names) {
  return function() {
    classedRemove(this, names);
  };
}

function classedFunction(names, value) {
  return function() {
    (value.apply(this, arguments) ? classedAdd : classedRemove)(this, names);
  };
}

var selection_classed = function(name, value) {
  var names = classArray(name + "");

  if (arguments.length < 2) {
    var list = classList(this.node()), i = -1, n = names.length;
    while (++i < n) if (!list.contains(names[i])) return false;
    return true;
  }

  return this.each((typeof value === "function"
      ? classedFunction : value
      ? classedTrue
      : classedFalse)(names, value));
};

function textRemove() {
  this.textContent = "";
}

function textConstant(value) {
  return function() {
    this.textContent = value;
  };
}

function textFunction(value) {
  return function() {
    var v = value.apply(this, arguments);
    this.textContent = v == null ? "" : v;
  };
}

var selection_text = function(value) {
  return arguments.length
      ? this.each(value == null
          ? textRemove : (typeof value === "function"
          ? textFunction
          : textConstant)(value))
      : this.node().textContent;
};

function htmlRemove() {
  this.innerHTML = "";
}

function htmlConstant(value) {
  return function() {
    this.innerHTML = value;
  };
}

function htmlFunction(value) {
  return function() {
    var v = value.apply(this, arguments);
    this.innerHTML = v == null ? "" : v;
  };
}

var selection_html = function(value) {
  return arguments.length
      ? this.each(value == null
          ? htmlRemove : (typeof value === "function"
          ? htmlFunction
          : htmlConstant)(value))
      : this.node().innerHTML;
};

function raise$1() {
  if (this.nextSibling) this.parentNode.appendChild(this);
}

var selection_raise = function() {
  return this.each(raise$1);
};

function lower() {
  if (this.previousSibling) this.parentNode.insertBefore(this, this.parentNode.firstChild);
}

var selection_lower = function() {
  return this.each(lower);
};

var selection_append = function(name) {
  var create = typeof name === "function" ? name : creator(name);
  return this.select(function() {
    return this.appendChild(create.apply(this, arguments));
  });
};

function constantNull() {
  return null;
}

var selection_insert = function(name, before) {
  var create = typeof name === "function" ? name : creator(name),
      select = before == null ? constantNull : typeof before === "function" ? before : selector(before);
  return this.select(function() {
    return this.insertBefore(create.apply(this, arguments), select.apply(this, arguments) || null);
  });
};

function remove() {
  var parent = this.parentNode;
  if (parent) parent.removeChild(this);
}

var selection_remove = function() {
  return this.each(remove);
};

var selection_datum = function(value) {
  return arguments.length
      ? this.property("__data__", value)
      : this.node().__data__;
};

function dispatchEvent(node, type, params) {
  var window = defaultView(node),
      event = window.CustomEvent;

  if (event) {
    event = new event(type, params);
  } else {
    event = window.document.createEvent("Event");
    if (params) event.initEvent(type, params.bubbles, params.cancelable), event.detail = params.detail;
    else event.initEvent(type, false, false);
  }

  node.dispatchEvent(event);
}

function dispatchConstant(type, params) {
  return function() {
    return dispatchEvent(this, type, params);
  };
}

function dispatchFunction(type, params) {
  return function() {
    return dispatchEvent(this, type, params.apply(this, arguments));
  };
}

var selection_dispatch = function(type, params) {
  return this.each((typeof params === "function"
      ? dispatchFunction
      : dispatchConstant)(type, params));
};

var root = [null];

function Selection(groups, parents) {
  this._groups = groups;
  this._parents = parents;
}

function selection() {
  return new Selection([[document.documentElement]], root);
}

Selection.prototype = selection.prototype = {
  constructor: Selection,
  select: selection_select,
  selectAll: selection_selectAll,
  filter: selection_filter,
  data: selection_data,
  enter: selection_enter,
  exit: selection_exit,
  merge: selection_merge,
  order: selection_order,
  sort: selection_sort,
  call: selection_call,
  nodes: selection_nodes,
  node: selection_node,
  size: selection_size,
  empty: selection_empty,
  each: selection_each,
  attr: selection_attr,
  style: selection_style,
  property: selection_property,
  classed: selection_classed,
  text: selection_text,
  html: selection_html,
  raise: selection_raise,
  lower: selection_lower,
  append: selection_append,
  insert: selection_insert,
  remove: selection_remove,
  datum: selection_datum,
  on: selection_on,
  dispatch: selection_dispatch
};

var select = function(selector) {
  return typeof selector === "string"
      ? new Selection([[document.querySelector(selector)]], [document.documentElement])
      : new Selection([[selector]], root);
};

class CalHeatMap {
    constructor() {
        "use strict";

        var self = this;

        this.allowedDataType = ["json", "csv", "tsv", "txt"];

        // Default settings
        this.options = {
            // selector string of the container to append the graph to
            // Accept any string value accepted by document.querySelector or CSS3
            // or an Element object
            itemSelector: "#cal-heatmap",

            // Whether to paint the calendar on init()
            // Used by testsuite to reduce testing time
            paintOnLoad: true,

            // ================================================
            // DOMAIN
            // ================================================

            // Number of domain to display on the graph
            range: 12,

            // Size of each cell, in pixel
            cellSize: 10,

            // Padding between each cell, in pixel
            cellPadding: 2,

            // For rounded subdomain rectangles, in pixels
            cellRadius: 0,

            domainGutter: 2,

            domainMargin: [0, 0, 0, 0],

            domain: "hour",

            subDomain: "min",

            // Number of columns to split the subDomains to
            // If not null, will takes precedence over rowLimit
            colLimit: null,

            // Number of rows to split the subDomains to
            // Will be ignored if colLimit is not null
            rowLimit: null,

            // First day of the week is Monday
            // 0 to start the week on Sunday
            weekStartOnMonday: true,

            // Start date of the graph
            // @default now
            start: new Date(),

            minDate: null,

            maxDate: null,

            // ================================================
            // DATA
            // ================================================

            // Data source
            // URL, where to fetch the original datas
            data: "",

            // Data type
            // Default: json
            dataType: this.allowedDataType[0],

            // Payload sent when using POST http method
            // Leave to null (default) for GET request
            // Expect a string, formatted like "a=b;c=d"
            dataPostPayload: null,

            // Whether to consider missing date:value from the datasource
            // as equal to 0, or just leave them as missing
            considerMissingDataAsZero: false,

            // Load remote data on calendar creation
            // When false, the calendar will be left empty
            loadOnInit: true,

            // Calendar orientation
            // false: display domains side by side
            // true : display domains one under the other
            verticalOrientation: false,

            // Domain dynamic width/height
            // The width on a domain depends on the number of
            domainDynamicDimension: true,

            // Domain Label properties
            label: {
                // valid: top, right, bottom, left
                position: "bottom",

                // Valid: left, center, right
                // Also valid are the direct svg values: start, middle, end
                align: "center",

                // By default, there is no margin/padding around the label
                offset: {
                    x: 0,
                    y: 0
                },

                rotate: null,

                // Used only on vertical orientation
                width: 100,

                // Used only on horizontal orientation
                height: null
            },

            // ================================================
            // LEGEND
            // ================================================

            // Threshold for the legend
            legend: [10, 20, 30, 40],

            // Whether to display the legend
            displayLegend: true,

            legendCellSize: 10,

            legendCellPadding: 2,

            legendMargin: [0, 0, 0, 0],

            // Legend vertical position
            // top: place legend above calendar
            // bottom: place legend below the calendar
            legendVerticalPosition: "bottom",

            // Legend horizontal position
            // accepted values: left, center, right
            legendHorizontalPosition: "left",

            // Legend rotation
            // accepted values: horizontal, vertical
            legendOrientation: "horizontal",

            // Objects holding all the heatmap different colors
            // null to disable, and use the default css styles
            //
            // Examples:
            // legendColors: {
            //		min: "green",
            //		max: "red",
            //		empty: "#ffffff",
            //		base: "grey",
            //		overflow: "red"
            // }
            legendColors: null,

            // ================================================
            // HIGHLIGHT
            // ================================================

            // List of dates to highlight
            // Valid values:
            // - []: don't highlight anything
            // - "now": highlight the current date
            // - an array of Date objects: highlight the specified dates
            highlight: [],

            // ================================================
            // TEXT FORMATTING / i18n
            // ================================================

            // Name of the items to represent in the calendar
            itemName: ["item", "items"],

            // Formatting of the domain label
            // @default: null, will use the formatting according to domain type
            // Accept a string used as specifier by d3.timeFormat()
            // or a function
            //
            // Refer to https://github.com/mbostock/d3/wiki/Time-Formatting
            // for accepted date formatting used by d3.timeFormat()
            domainLabelFormat: null,

            // Formatting of the title displayed when hovering a subDomain cell
            subDomainTitleFormat: {
                empty: "{date}",
                filled: "{count} {name} {connector} {date}"
            },

            // Formatting of the {date} used in subDomainTitleFormat
            // @default: null, will use the formatting according to subDomain type
            // Accept a string used as specifier by d3.timeFormat()
            // or a function
            //
            // Refer to https://github.com/mbostock/d3/wiki/Time-Formatting
            // for accepted date formatting used by d3.timeFormat()
            subDomainDateFormat: null,

            // Formatting of the text inside each subDomain cell
            // @default: null, no text
            // Accept a string used as specifier by d3.timeFormat()
            // or a function
            //
            // Refer to https://github.com/mbostock/d3/wiki/Time-Formatting
            // for accepted date formatting used by d3.timeFormat()
            subDomainTextFormat: null,

            // Formatting of the title displayed when hovering a legend cell
            legendTitleFormat: {
                lower: "less than {min} {name}",
                inner: "between {down} and {up} {name}",
                upper: "more than {max} {name}"
            },

            // Animation duration, in ms
            animationDuration: 500,

            nextSelector: false,

            previousSelector: false,

            itemNamespace: "cal-heatmap",

            tooltip: false,

            // ================================================
            // EVENTS CALLBACK
            // ================================================

            // Callback when clicking on a time block
            onClick: null,

            // Callback after painting the empty calendar
            // Can be used to trigger an API call, once the calendar is ready to be filled
            afterLoad: null,

            // Callback after loading the next domain in the calendar
            afterLoadNextDomain: null,

            // Callback after loading the previous domain in the calendar
            afterLoadPreviousDomain: null,

            // Callback after finishing all actions on the calendar
            onComplete: null,

            // Callback after fetching the datas, but before applying them to the calendar
            // Used mainly to convert the datas if they're not formatted like expected
            // Takes the fetched "data" object as argument, must return a json object
            // formatted like {timestamp:count, timestamp2:count2},
            afterLoadData(data) {
                return data;
            },

            // Callback triggered after calling next().
            // The `status` argument is equal to true if there is no
            // more next domain to load
            //
            // This callback is also executed once, after calling previous(),
            // only when the max domain is reached
            onMaxDomainReached: null,

            // Callback triggered after calling previous().
            // The `status` argument is equal to true if there is no
            // more previous domain to load
            //
            // This callback is also executed once, after calling next(),
            // only when the min domain is reached
            onMinDomainReached: null
        };

        this._domainType = {
            "min": {
                name: "minute",
                level: 10,
                maxItemNumber: 60,
                defaultRowNumber: 10,
                defaultColumnNumber: 6,
                row(d) {
                    return self.getSubDomainRowNumber(d);
                },
                column(d) {
                    return self.getSubDomainColumnNumber(d);
                },
                position: {
                    x(d) {
                        return Math.floor(d.getMinutes() / self._domainType.min.row(d));
                    },
                    y(d) {
                        return d.getMinutes() % self._domainType.min.row(d);
                    }
                },
                format: {
                    date: "%H:%M, %A %B %-e, %Y",
                    legend: "",
                    connector: "at"
                },
                extractUnit(d) {
                    return new Date(d.getFullYear(), d.getMonth(), d.getDate(), d.getHours(), d.getMinutes()).getTime();
                }
            },
            "hour": {
                name: "hour",
                level: 20,
                maxItemNumber(d) {
                    switch (self.options.domain) {
                        case "day":
                            return 24;
                        case "week":
                            return 24 * 7;
                        case "month":
                            return 24 * (self.options.domainDynamicDimension ? self.getDayCountInMonth(d) : 31);
                    }
                },
                defaultRowNumber: 6,
                defaultColumnNumber(d) {
                    switch (self.options.domain) {
                        case "day":
                            return 4;
                        case "week":
                            return 28;
                        case "month":
                            return self.options.domainDynamicDimension ? self.getDayCountInMonth(d) : 31;
                    }
                },
                row(d) {
                    return self.getSubDomainRowNumber(d);
                },
                column(d) {
                    return self.getSubDomainColumnNumber(d);
                },
                position: {
                    x(d) {
                        if (self.options.domain === "month") {
                            if (self.options.colLimit > 0 || self.options.rowLimit > 0) {
                                return Math.floor((d.getHours() + (d.getDate() - 1) * 24) / self._domainType.hour.row(d));
                            }
                            return Math.floor(d.getHours() / self._domainType.hour.row(d)) + (d.getDate() - 1) * 4;
                        } else if (self.options.domain === "week") {
                            if (self.options.colLimit > 0 || self.options.rowLimit > 0) {
                                return Math.floor((d.getHours() + self.getWeekDay(d) * 24) / self._domainType.hour.row(d));
                            }
                            return Math.floor(d.getHours() / self._domainType.hour.row(d)) + self.getWeekDay(d) * 4;
                        }
                        return Math.floor(d.getHours() / self._domainType.hour.row(d));
                    },
                    y(d) {
                        var p = d.getHours();
                        if (self.options.colLimit > 0 || self.options.rowLimit > 0) {
                            switch (self.options.domain) {
                                case "month":
                                    p += (d.getDate() - 1) * 24;
                                    break;
                                case "week":
                                    p += self.getWeekDay(d) * 24;
                                    break;
                            }
                        }
                        return Math.floor(p % self._domainType.hour.row(d));
                    }
                },
                format: {
                    date: "%Hh, %A %B %-e, %Y",
                    legend: "%H:00",
                    connector: "at"
                },
                extractUnit(d) {
                    return new Date(d.getFullYear(), d.getMonth(), d.getDate(), d.getHours()).getTime();
                }
            },
            "day": {
                name: "day",
                level: 30,
                maxItemNumber(d) {
                    switch (self.options.domain) {
                        case "week":
                            return 7;
                        case "month":
                            return self.options.domainDynamicDimension ? self.getDayCountInMonth(d) : 31;
                        case "year":
                            return self.options.domainDynamicDimension ? self.getDayCountInYear(d) : 366;
                    }
                },
                defaultColumnNumber(d) {
                    d = new Date(d);
                    switch (self.options.domain) {
                        case "week":
                            return 1;
                        case "month":
                            return (self.options.domainDynamicDimension && !self.options.verticalOrientation) ? (self.getWeekNumber(new Date(d.getFullYear(), d.getMonth() + 1, 0)) - self.getWeekNumber(d) + 1) : 6;
                        case "year":
                            return (self.options.domainDynamicDimension ? (self.getWeekNumber(new Date(d.getFullYear(), 11, 31)) - self.getWeekNumber(new Date(d.getFullYear(), 0)) + 1) : 54);
                    }
                },
                defaultRowNumber: 7,
                row(d) {
                    return self.getSubDomainRowNumber(d);
                },
                column(d) {
                    return self.getSubDomainColumnNumber(d);
                },
                position: {
                    x(d) {
                        switch (self.options.domain) {
                            case "week":
                                return Math.floor(self.getWeekDay(d) / self._domainType.day.row(d));
                            case "month":
                                if (self.options.colLimit > 0 || self.options.rowLimit > 0) {
                                    return Math.floor((d.getDate() - 1) / self._domainType.day.row(d));
                                }
                                return self.getWeekNumber(d) - self.getWeekNumber(new Date(d.getFullYear(), d.getMonth()));
                            case "year":
                                if (self.options.colLimit > 0 || self.options.rowLimit > 0) {
                                    return Math.floor((self.getDayOfYear(d) - 1) / self._domainType.day.row(d));
                                }
                                return self.getWeekNumber(d);
                        }
                    },
                    y(d) {
                        var p = self.getWeekDay(d);
                        if (self.options.colLimit > 0 || self.options.rowLimit > 0) {
                            switch (self.options.domain) {
                                case "year":
                                    p = self.getDayOfYear(d) - 1;
                                    break;
                                case "week":
                                    p = self.getWeekDay(d);
                                    break;
                                case "month":
                                    p = d.getDate() - 1;
                                    break;
                            }
                        }
                        return Math.floor(p % self._domainType.day.row(d));
                    }
                },
                format: {
                    date: "%A %B %-e, %Y",
                    legend: "%e %b",
                    connector: "on"
                },
                extractUnit(d) {
                    return new Date(d.getFullYear(), d.getMonth(), d.getDate()).getTime();
                }
            },
            "week": {
                name: "week",
                level: 40,
                maxItemNumber: 54,
                defaultColumnNumber(d) {
                    d = new Date(d);
                    switch (self.options.domain) {
                        case "year":
                            return self._domainType.week.maxItemNumber;
                        case "month":
                            return self.options.domainDynamicDimension ? self.getWeekNumber(new Date(d.getFullYear(), d.getMonth() + 1, 0)) - self.getWeekNumber(d) : 5;
                    }
                },
                defaultRowNumber: 1,
                row(d) {
                    return self.getSubDomainRowNumber(d);
                },
                column(d) {
                    return self.getSubDomainColumnNumber(d);
                },
                position: {
                    x(d) {
                        switch (self.options.domain) {
                            case "year":
                                return Math.floor(self.getWeekNumber(d) / self._domainType.week.row(d));
                            case "month":
                                return Math.floor(self.getMonthWeekNumber(d) / self._domainType.week.row(d));
                        }
                    },
                    y(d) {
                        return self.getWeekNumber(d) % self._domainType.week.row(d);
                    }
                },
                format: {
                    date: "%B Week #%W",
                    legend: "%B Week #%W",
                    connector: "in"
                },
                extractUnit(d) {
                    var dt = new Date(d.getFullYear(), d.getMonth(), d.getDate());
                    // According to ISO-8601, week number computation are based on week starting on Monday
                    var weekDay = dt.getDay() - (self.options.weekStartOnMonday ? 1 : 0);
                    if (weekDay < 0) {
                        weekDay = 6;
                    }
                    dt.setDate(dt.getDate() - weekDay);
                    return dt.getTime();
                }
            },
            "month": {
                name: "month",
                level: 50,
                maxItemNumber: 12,
                defaultColumnNumber: 12,
                defaultRowNumber: 1,
                row() {
                    return self.getSubDomainRowNumber();
                },
                column() {
                    return self.getSubDomainColumnNumber();
                },
                position: {
                    x(d) {
                        return Math.floor(d.getMonth() / self._domainType.month.row(d));
                    },
                    y(d) {
                        return d.getMonth() % self._domainType.month.row(d);
                    }
                },
                format: {
                    date: "%B %Y",
                    legend: "%B",
                    connector: "in"
                },
                extractUnit(d) {
                    return new Date(d.getFullYear(), d.getMonth()).getTime();
                }
            },
            "year": {
                name: "year",
                level: 60,
                row() {
                    return self.options.rowLimit || 1;
                },
                column() {
                    return self.options.colLimit || 1;
                },
                position: {
                    x() {
                        return 1;
                    },
                    y() {
                        return 1;
                    }
                },
                format: {
                    date: "%Y",
                    legend: "%Y",
                    connector: "in"
                },
                extractUnit(d) {
                    return new Date(d.getFullYear()).getTime();
                }
            }
        };

        for (var type in this._domainType) {
            if (this._domainType.hasOwnProperty(type)) {
                var d = this._domainType[type];
                this._domainType["x_" + type] = {
                    name: "x_" + type,
                    level: d.type,
                    maxItemNumber: d.maxItemNumber,
                    defaultRowNumber: d.defaultRowNumber,
                    defaultColumnNumber: d.defaultColumnNumber,
                    row: d.column,
                    column: d.row,
                    position: {
                        x: d.position.y,
                        y: d.position.x
                    },
                    format: d.format,
                    extractUnit: d.extractUnit
                };
            }
        }

        // Record the address of the last inserted domain when browsing
        this.lastInsertedSvg = null;

        this._completed = false;

        // Record all the valid domains
        // Each domain value is a timestamp in milliseconds
        this._domains = map$1();

        this.graphDim = {
            width: 0,
            height: 0
        };

        this.legendDim = {
            width: 0,
            height: 0
        };

        this.NAVIGATE_LEFT = 1;
        this.NAVIGATE_RIGHT = 2;

        // Various update mode when using the update() API
        this.RESET_ALL_ON_UPDATE = 0;
        this.RESET_SINGLE_ON_UPDATE = 1;
        this.APPEND_ON_UPDATE = 2;

        this.DEFAULT_LEGEND_MARGIN = 10;

        this.root = null;
        this.tooltip = null;

        this._maxDomainReached = false;
        this._minDomainReached = false;

        this.domainPosition = new DomainPosition();
        this.Legend = null;
        this.legendScale = null;

        // List of domains that are skipped because of DST
        // All times belonging to these domains should be re-assigned to the previous domain
        this.DSTDomain = [];

        /**
         * Display the graph for the first time
         * @return bool True if the calendar is created
         */
        this._init = function() {

            self.getDomain(self.options.start).map(function(d) {
                return d.getTime();
            }).map(function(d) {
                self._domains.set(d, self.getSubDomain(d).map(function(d) {
                    return { t: self._domainType[self.options.subDomain].extractUnit(d), v: null };
                }));
            });

            self.root = select(self.options.itemSelector).append("svg").attr("class", "cal-heatmap-container");

            self.tooltip = select(self.options.itemSelector)
                .attr("style", function() {
                    var current = select(self.options.itemSelector).attr("style");
                    return (current !== null ? current : "") + "position:relative;";
                })
                .append("div")
                .attr("class", "ch-tooltip");

            self.root.attr("x", 0).attr("y", 0).append("svg").attr("class", "graph");

            self.Legend = new Legend(self);

            if (self.options.paintOnLoad) {
                _initCalendar();
            }

            return true;
        };

        function _initCalendar() {
            self.verticalDomainLabel = (self.options.label.position === "top" || self.options.label.position === "bottom");

            self.domainVerticalLabelHeight = self.options.label.height === null ? Math.max(25, self.options.cellSize * 2) : self.options.label.height;
            self.domainHorizontalLabelWidth = 0;

            if (self.options.domainLabelFormat === "" && self.options.label.height === null) {
                self.domainVerticalLabelHeight = 0;
            }

            if (!self.verticalDomainLabel) {
                self.domainVerticalLabelHeight = 0;
                self.domainHorizontalLabelWidth = self.options.label.width;
            }

            self.paint();

            // =========================================================================//
            // ATTACHING DOMAIN NAVIGATION EVENT										//
            // =========================================================================//
            if (self.options.nextSelector !== false) {
                select(self.options.nextSelector).on("click." + self.options.itemNamespace, function() {
                    event.preventDefault();
                    return self.loadNextDomain(1);
                });
            }

            if (self.options.previousSelector !== false) {
                select(self.options.previousSelector).on("click." + self.options.itemNamespace, function() {
                    event.preventDefault();
                    return self.loadPreviousDomain(1);
                });
            }

            self.Legend.redraw(self.graphDim.width - self.options.domainGutter - self.options.cellPadding);
            self.afterLoad();

            var domains = self.getDomainKeys();

            // Fill the graph with some datas
            if (self.options.loadOnInit) {
                self.getDatas(
                    self.options.data,
                    new Date(domains[0]),
                    self.getSubDomain(domains[domains.length - 1]).pop(),
                    function() {
                        self.fill();
                        self.onComplete();
                    }
                );
            } else {
                self.onComplete();
            }

            self.checkIfMinDomainIsReached(domains[0]);
            self.checkIfMaxDomainIsReached(self.getNextDomain().getTime());
        }

        // Return the width of the domain block, without the domain gutter
        // @param int d Domain start timestamp
        function w(d, outer) {
            var width = self.options.cellSize * self._domainType[self.options.subDomain].column(d) + self.options.cellPadding * self._domainType[self.options.subDomain].column(d);
            if (arguments.length === 2 && outer === true) {
                return width += self.domainHorizontalLabelWidth + self.options.domainGutter + self.options.domainMargin[1] + self.options.domainMargin[3];
            }
            return width;
        }

        // Return the height of the domain block, without the domain gutter
        function h(d, outer) {
            var height = self.options.cellSize * self._domainType[self.options.subDomain].row(d) + self.options.cellPadding * self._domainType[self.options.subDomain].row(d);
            if (arguments.length === 2 && outer === true) {
                height += self.options.domainGutter + self.domainVerticalLabelHeight + self.options.domainMargin[0] + self.options.domainMargin[2];
            }
            return height;
        }

        /**
         *
         *
         * @param int navigationDir
         */
        this.paint = function(navigationDir) {

            var options = self.options;

            if (arguments.length === 0) {
                navigationDir = false;
            }

            // Painting all the domains
            var domainSvg = self.root.select(".graph")
                .selectAll(".graph-domain")
                .data(
                    function() {
                        var data = self.getDomainKeys();
                        return navigationDir === self.NAVIGATE_LEFT ? data.reverse() : data;
                    },
                    function(d) {
                        return d;
                    }
                );

            var enteringDomainDim = 0;
            var exitingDomainDim = 0;

            // =========================================================================//
            // PAINTING DOMAIN															//
            // =========================================================================//

            var svg = domainSvg
                .enter()
                .append("svg")
                .attr("width", function(d) {
                    return w(d, true);
                })
                .attr("height", function(d) {
                    return h(d, true);
                })
                .attr("x", function(d) {
                    if (options.verticalOrientation) {
                        self.graphDim.width = Math.max(self.graphDim.width, w(d, true));
                        return 0;
                    } else {
                        return getDomainPosition(d, self.graphDim, "width", w(d, true));
                    }
                })
                .attr("y", function(d) {
                    if (options.verticalOrientation) {
                        return getDomainPosition(d, self.graphDim, "height", h(d, true));
                    } else {
                        self.graphDim.height = Math.max(self.graphDim.height, h(d, true));
                        return 0;
                    }
                })
                .attr("class", function(d) {
                    var classname = "graph-domain";
                    var date = new Date(d);
                    switch (options.domain) {
                        case "hour":
                            classname += " h_" + date.getHours();
                            /* falls through */
                        case "day":
                            classname += " d_" + date.getDate() + " dy_" + date.getDay();
                            /* falls through */
                        case "week":
                            classname += " w_" + self.getWeekNumber(date);
                            /* falls through */
                        case "month":
                            classname += " m_" + (date.getMonth() + 1);
                            /* falls through */
                        case "year":
                            classname += " y_" + date.getFullYear();
                    }
                    return classname;
                });

            self.lastInsertedSvg = svg;

            function getDomainPosition(domainIndex, graphDim, axis, domainDim) {
                var tmp = 0;
                switch (navigationDir) {
                    case false:
                        tmp = graphDim[axis];

                        graphDim[axis] += domainDim;
                        self.domainPosition.setPosition(domainIndex, tmp);
                        return tmp;

                    case self.NAVIGATE_RIGHT:
                        self.domainPosition.setPosition(domainIndex, graphDim[axis]);

                        enteringDomainDim = domainDim;
                        exitingDomainDim = self.domainPosition.getPositionFromIndex(1);

                        self.domainPosition.shiftRightBy(exitingDomainDim);
                        return graphDim[axis];

                    case self.NAVIGATE_LEFT:
                        tmp = -domainDim;

                        enteringDomainDim = -tmp;
                        exitingDomainDim = graphDim[axis] - self.domainPosition.getLast();

                        self.domainPosition.setPosition(domainIndex, tmp);
                        self.domainPosition.shiftLeftBy(enteringDomainDim);
                        return tmp;
                }
            }

            svg.append("rect")
                .attr("width", function(d) {
                    return w(d, true) - options.domainGutter - options.cellPadding;
                })
                .attr("height", function(d) {
                    return h(d, true) - options.domainGutter - options.cellPadding;
                })
                .attr("class", "domain-background");

            // =========================================================================//
            // PAINTING SUBDOMAINS														//
            // =========================================================================//
            var subDomainSvgGroup = svg.append("svg")
                .attr("x", function() {
                    if (options.label.position === "left") {
                        return self.domainHorizontalLabelWidth + options.domainMargin[3];
                    } else {
                        return options.domainMargin[3];
                    }
                })
                .attr("y", function() {
                    if (options.label.position === "top") {
                        return self.domainVerticalLabelHeight + options.domainMargin[0];
                    } else {
                        return options.domainMargin[0];
                    }
                })
                .attr("class", "graph-subdomain-group");

            var rect = subDomainSvgGroup
                .selectAll("g")
                .data(function(d) {
                    return self._domains.get(d);
                })
                .enter()
                .append("g");

            rect
                .append("rect")
                .attr("class", function(d) {
                    return "graph-rect" + self.getHighlightClassName(d.t) + (options.onClick !== null ? " hover_cursor" : "");
                })
                .attr("width", options.cellSize)
                .attr("height", options.cellSize)
                .attr("x", function(d) {
                    return self.positionSubDomainX(d.t);
                })
                .attr("y", function(d) {
                    return self.positionSubDomainY(d.t);
                })
                .on("click", function(d) {
                    if (options.onClick !== null) {
                        return self.onClick(new Date(d.t), d.v);
                    }
                })
                .call(function(selection$$1) {
                    if (options.cellRadius > 0) {
                        selection$$1
                            .attr("rx", options.cellRadius)
                            .attr("ry", options.cellRadius);
                    }

                    if (self.legendScale !== null && options.legendColors !== null && options.legendColors.hasOwnProperty("base")) {
                        selection$$1.attr("fill", options.legendColors.base);
                    }

                    if (options.tooltip) {
                        selection$$1.on("mouseover", function(d) {
                            var domainNode = this.parentNode.parentNode;

                            self.tooltip
                                .html(self.getSubDomainTitle(d))
                                .attr("style", "display: block;");

                            var tooltipPositionX = self.positionSubDomainX(d.t) - self.tooltip.node().offsetWidth / 2 + options.cellSize / 2;
                            var tooltipPositionY = self.positionSubDomainY(d.t) - self.tooltip.node().offsetHeight - options.cellSize / 2;

                            // Offset by the domain position
                            tooltipPositionX += parseInt(domainNode.getAttribute("x"), 10);
                            tooltipPositionY += parseInt(domainNode.getAttribute("y"), 10);

                            // Offset by the calendar position (when legend is left/top)
                            tooltipPositionX += parseInt(self.root.select(".graph").attr("x"), 10);
                            tooltipPositionY += parseInt(self.root.select(".graph").attr("y"), 10);

                            // Offset by the inside domain position (when label is left/top)
                            tooltipPositionX += parseInt(domainNode.parentNode.getAttribute("x"), 10);
                            tooltipPositionY += parseInt(domainNode.parentNode.getAttribute("y"), 10);

                            self.tooltip.attr("style",
                                "display: block; " +
                                "left: " + tooltipPositionX + "px; " +
                                "top: " + tooltipPositionY + "px;");
                        });

                        selection$$1.on("mouseout", function() {
                            self.tooltip
                                .attr("style", "display:none")
                                .html("");
                        });
                    }
                });

            // Appending a title to each subdomain
            if (!options.tooltip) {
                rect.append("title").text(function(d) {
                    return self.formatDate(new Date(d.t), options.subDomainDateFormat);
                });
            }

            // =========================================================================//
            // PAINTING LABEL															//
            // =========================================================================//
            if (options.domainLabelFormat !== "") {
                svg.append("text")
                    .attr("class", "graph-label")
                    .attr("y", function(d) {
                        var y = options.domainMargin[0];
                        switch (options.label.position) {
                            case "top":
                                y += self.domainVerticalLabelHeight / 2;
                                break;
                            case "bottom":
                                y += h(d) + self.domainVerticalLabelHeight / 2;
                        }

                        return y + options.label.offset.y *
                            (
                                ((options.label.rotate === "right" && options.label.position === "right") ||
                                    (options.label.rotate === "left" && options.label.position === "left")) ?
                                -1 : 1
                            );
                    })
                    .attr("x", function(d) {
                        var x = options.domainMargin[3];
                        switch (options.label.position) {
                            case "right":
                                x += w(d);
                                break;
                            case "bottom":
                            case "top":
                                x += w(d) / 2;
                        }

                        if (options.label.align === "right") {
                            return x + self.domainHorizontalLabelWidth - options.label.offset.x *
                                (options.label.rotate === "right" ? -1 : 1);
                        }
                        return x + options.label.offset.x;

                    })
                    .attr("text-anchor", function() {
                        switch (options.label.align) {
                            case "start":
                            case "left":
                                return "start";
                            case "end":
                            case "right":
                                return "end";
                            default:
                                return "middle";
                        }
                    })
                    .attr("dominant-baseline", function() {
                        return self.verticalDomainLabel ? "middle" : "top";
                    })
                    .text(function(d) {
                        return self.formatDate(new Date(d), options.domainLabelFormat);
                    })
                    .call(domainRotate);
            }

            function domainRotate(selection$$1) {
                switch (options.label.rotate) {
                    case "right":
                        selection$$1
                            .attr("transform", function(d) {
                                var s = "rotate(90), ";
                                switch (options.label.position) {
                                    case "right":
                                        s += "translate(-" + w(d) + " , -" + w(d) + ")";
                                        break;
                                    case "left":
                                        s += "translate(0, -" + self.domainHorizontalLabelWidth + ")";
                                        break;
                                }

                                return s;
                            });
                        break;
                    case "left":
                        selection$$1
                            .attr("transform", function(d) {
                                var s = "rotate(270), ";
                                switch (options.label.position) {
                                    case "right":
                                        s += "translate(-" + (w(d) + self.domainHorizontalLabelWidth) + " , " + w(d) + ")";
                                        break;
                                    case "left":
                                        s += "translate(-" + (self.domainHorizontalLabelWidth) + " , " + self.domainHorizontalLabelWidth + ")";
                                        break;
                                }

                                return s;
                            });
                        break;
                }
            }

            // =========================================================================//
            // PAINTING DOMAIN SUBDOMAIN CONTENT										//
            // =========================================================================//
            if (options.subDomainTextFormat !== null) {
                rect
                    .append("text")
                    .attr("class", function(d) {
                        return "subdomain-text" + self.getHighlightClassName(d.t);
                    })
                    .attr("x", function(d) {
                        return self.positionSubDomainX(d.t) + options.cellSize / 2;
                    })
                    .attr("y", function(d) {
                        return self.positionSubDomainY(d.t) + options.cellSize / 2;
                    })
                    .attr("text-anchor", "middle")
                    .attr("dominant-baseline", "central")
                    .text(function(d) {
                        return self.formatDate(new Date(d.t), options.subDomainTextFormat);
                    });
            }

            // =========================================================================//
            // ANIMATION																//
            // =========================================================================//

            if (navigationDir !== false) {
                domainSvg.transition().duration(options.animationDuration)
                    .attr("x", function(d) {
                        return options.verticalOrientation ? 0 : self.domainPosition.getPosition(d);
                    })
                    .attr("y", function(d) {
                        return options.verticalOrientation ? self.domainPosition.getPosition(d) : 0;
                    });
            }

            var tempWidth = self.graphDim.width;
            var tempHeight = self.graphDim.height;

            if (options.verticalOrientation) {
                self.graphDim.height += enteringDomainDim - exitingDomainDim;
            } else {
                self.graphDim.width += enteringDomainDim - exitingDomainDim;
            }

            // At the time of exit, domainsWidth and domainsHeight already automatically shifted
            domainSvg.exit().transition().duration(options.animationDuration)
                .attr("x", function(d) {
                    if (options.verticalOrientation) {
                        return 0;
                    } else {
                        switch (navigationDir) {
                            case self.NAVIGATE_LEFT:
                                return Math.min(self.graphDim.width, tempWidth);
                            case self.NAVIGATE_RIGHT:
                                return -w(d, true);
                        }
                    }
                })
                .attr("y", function(d) {
                    if (options.verticalOrientation) {
                        switch (navigationDir) {
                            case self.NAVIGATE_LEFT:
                                return Math.min(self.graphDim.height, tempHeight);
                            case self.NAVIGATE_RIGHT:
                                return -h(d, true);
                        }
                    } else {
                        return 0;
                    }
                })
                .remove();

            // Resize the root container
            self.resize();
        };
    }

    /**
     * Validate and merge user settings with default settings
     *
     * @param  {object} settings User settings
     * @return {bool} False if settings contains error
     */
    /* jshint maxstatements:false */
    init(settings) {
        "use strict";

        var parent = this;

        var options = parent.options = mergeRecursive(parent.options, settings);

        // Fatal errors
        // Stop script execution on error
        validateDomainType();
        validateSelector(options.itemSelector, false, "itemSelector");

        if (parent.allowedDataType.indexOf(options.dataType) === -1) {
            throw new Error("The data type '" + options.dataType + "' is not valid data type");
        }

        if (select(options.itemSelector).node() === null) {
            throw new Error("The node '" + options.itemSelector + "' specified in itemSelector does not exists");
        }

        try {
            validateSelector(options.nextSelector, true, "nextSelector");
            validateSelector(options.previousSelector, true, "previousSelector");
        } catch (error) {
            console.log(error.message);
            return false;
        }

        // If other settings contains error, will fallback to default

        if (!settings.hasOwnProperty("subDomain")) {
            this.options.subDomain = getOptimalSubDomain(settings.domain);
        }

        if (typeof options.itemNamespace !== "string" || options.itemNamespace === "") {
            console.log("itemNamespace can not be empty, falling back to cal-heatmap");
            options.itemNamespace = "cal-heatmap";
        }

        // Don't touch these settings
        var s = ["data", "onComplete", "onClick", "afterLoad", "afterLoadData", "afterLoadPreviousDomain", "afterLoadNextDomain"];

        for (var k in s) {
            if (settings.hasOwnProperty(s[k])) {
                options[s[k]] = settings[s[k]];
            }
        }

        options.subDomainDateFormat = (typeof options.subDomainDateFormat === "string" || typeof options.subDomainDateFormat === "function" ? options.subDomainDateFormat : this._domainType[options.subDomain].format.date);
        options.domainLabelFormat = (typeof options.domainLabelFormat === "string" || typeof options.domainLabelFormat === "function" ? options.domainLabelFormat : this._domainType[options.domain].format.legend);
        options.subDomainTextFormat = ((typeof options.subDomainTextFormat === "string" && options.subDomainTextFormat !== "") || typeof options.subDomainTextFormat === "function" ? options.subDomainTextFormat : null);
        options.domainMargin = expandMarginSetting(options.domainMargin);
        options.legendMargin = expandMarginSetting(options.legendMargin);
        options.highlight = parent.expandDateSetting(options.highlight);
        options.itemName = expandItemName(options.itemName);
        options.colLimit = parseColLimit(options.colLimit);
        options.rowLimit = parseRowLimit(options.rowLimit);
        if (!settings.hasOwnProperty("legendMargin")) {
            autoAddLegendMargin();
        }
        autoAlignLabel();

        /**
         * Validate that a queryString is valid
         *
         * @param  {Element|string|bool} selector   The queryString to test
         * @param  {bool}	canBeFalse	Whether false is an accepted and valid value
         * @param  {string} name		Name of the tested selector
         * @throws {Error}				If the selector is not valid
         * @return {bool}				True if the selector is a valid queryString
         */
        function validateSelector(selector$$1, canBeFalse, name) {
            if (((canBeFalse && selector$$1 === false) || selector$$1 instanceof Element || typeof selector$$1 === "string") && selector$$1 !== "") {
                return true;
            }
            throw new Error("The " + name + " is not valid");
        }

        /**
         * Return the optimal subDomain for the specified domain
         *
         * @param  {string} domain a domain name
         * @return {string}        the subDomain name
         */
        function getOptimalSubDomain(domain) {
            switch (domain) {
                case "year":
                    return "month";
                case "month":
                    return "day";
                case "week":
                    return "day";
                case "day":
                    return "hour";
                default:
                    return "min";
            }
        }

        /**
         * Ensure that the domain and subdomain are valid
         *
         * @throw {Error} when domain or subdomain are not valid
         * @return {bool} True if domain and subdomain are valid and compatible
         */
        function validateDomainType() {
            if (!parent._domainType.hasOwnProperty(options.domain) || options.domain === "min" || options.domain.substring(0, 2) === "x_") {
                throw new Error("The domain '" + options.domain + "' is not valid");
            }

            if (!parent._domainType.hasOwnProperty(options.subDomain) || options.subDomain === "year") {
                throw new Error("The subDomain '" + options.subDomain + "' is not valid");
            }

            if (parent._domainType[options.domain].level <= parent._domainType[options.subDomain].level) {
                throw new Error("'" + options.subDomain + "' is not a valid subDomain to '" + options.domain + "'");
            }

            return true;
        }

        /**
         * Fine-tune the label alignement depending on its position
         *
         * @return void
         */
        function autoAlignLabel() {
            // Auto-align label, depending on it's position
            if (!settings.hasOwnProperty("label") || (settings.hasOwnProperty("label") && !settings.label.hasOwnProperty("align"))) {
                switch (options.label.position) {
                    case "left":
                        options.label.align = "right";
                        break;
                    case "right":
                        options.label.align = "left";
                        break;
                    default:
                        options.label.align = "center";
                }

                if (options.label.rotate === "left") {
                    options.label.align = "right";
                } else if (options.label.rotate === "right") {
                    options.label.align = "left";
                }
            }

            if (!settings.hasOwnProperty("label") || (settings.hasOwnProperty("label") && !settings.label.hasOwnProperty("offset"))) {
                if (options.label.position === "left" || options.label.position === "right") {
                    options.label.offset = {
                        x: 10,
                        y: 15
                    };
                }
            }
        }

        /**
         * If not specified, add some margin around the legend depending on its position
         *
         * @return void
         */
        function autoAddLegendMargin() {
            switch (options.legendVerticalPosition) {
                case "top":
                    options.legendMargin[2] = parent.DEFAULT_LEGEND_MARGIN;
                    break;
                case "bottom":
                    options.legendMargin[0] = parent.DEFAULT_LEGEND_MARGIN;
                    break;
                case "middle":
                case "center":
                    options.legendMargin[options.legendHorizontalPosition === "right" ? 3 : 1] = parent.DEFAULT_LEGEND_MARGIN;
            }
        }

        /**
         * Expand a number of an array of numbers to an usable 4 values array
         *
         * @param  {integer|array} value
         * @return {array}        array
         */
        function expandMarginSetting(value) {
            if (typeof value === "number") {
                value = [value];
            }

            if (!Array.isArray(value)) {
                console.log("Margin only takes an integer or an array of integers");
                value = [0];
            }

            switch (value.length) {
                case 1:
                    return [value[0], value[0], value[0], value[0]];
                case 2:
                    return [value[0], value[1], value[0], value[1]];
                case 3:
                    return [value[0], value[1], value[2], value[1]];
                case 4:
                    return value;
                default:
                    return value.slice(0, 4);
            }
        }

        /**
         * Convert a string to an array like [singular-form, plural-form]
         *
         * @param  {string|array} value Date to convert
         * @return {array}       An array like [singular-form, plural-form]
         */
        function expandItemName(value) {
            if (typeof value === "string") {
                return [value, value + (value !== "" ? "s" : "")];
            }

            if (Array.isArray(value)) {
                if (value.length === 1) {
                    return [value[0], value[0] + "s"];
                } else if (value.length > 2) {
                    return value.slice(0, 2);
                }

                return value;
            }

            return ["item", "items"];
        }

        function parseColLimit(value) {
            return value > 0 ? value : null;
        }

        function parseRowLimit(value) {
            if (value > 0 && options.colLimit > 0) {
                console.log("colLimit and rowLimit are mutually exclusive, rowLimit will be ignored");
                return null;
            }
            return value > 0 ? value : null;
        }

        return this._init();

    }

    /**
     * Convert a keyword or an array of keyword/date to an array of date objects
     *
     * @param  {string|array|Date} value Data to convert
     * @return {array}       An array of Dates
     */
    expandDateSetting(value) {
        "use strict";

        if (!Array.isArray(value)) {
            value = [value];
        }

        return value.map(function(data) {
            if (data === "now") {
                return new Date();
            }
            if (data instanceof Date) {
                return data;
            }
            return false;
        }).filter(function(d) {
            return d !== false;
        });
    }

    /**
     * Fill the calendar by coloring the cells
     *
     * @param array svg An array of html node to apply the transformation to (optional)
     *                  It's used to limit the painting to only a subset of the calendar
     * @return void
     */
    fill(svg) {
        "use strict";

        var parent = this;
        var options = parent.options;

        if (arguments.length === 0) {
            svg = parent.root.selectAll(".graph-domain");
        }

        var rect = svg
            .selectAll("svg").selectAll("g")
            .data(function(d) {
                return parent._domains.get(d);
            });

        /**
         * Colorize the cell via a style attribute if enabled
         */
        function addStyle(element) {
            if (parent.legendScale === null) {
                return false;
            }

            element.attr("fill", function(d) {
                if (d.v === null && (options.hasOwnProperty("considerMissingDataAsZero") && !options.considerMissingDataAsZero)) {
                    if (options.legendColors.hasOwnProperty("base")) {
                        return options.legendColors.base;
                    }
                }

                if (options.legendColors !== null && options.legendColors.hasOwnProperty("empty") &&
                    (d.v === 0 || (d.v === null && options.hasOwnProperty("considerMissingDataAsZero") && options.considerMissingDataAsZero))
                ) {
                    return options.legendColors.empty;
                }

                if (d.v < 0 && options.legend[0] > 0 && options.legendColors !== null && options.legendColors.hasOwnProperty("overflow")) {
                    return options.legendColors.overflow;
                }

                return parent.legendScale(Math.min(d.v, options.legend[options.legend.length - 1]));
            });
        }

        rect.transition().duration(options.animationDuration).select("rect")
            .attr("class", function(d) {

                var htmlClass = parent.getHighlightClassName(d.t).trim().split(" ");
                var pastDate = parent.dateIsLessThan(d.t, new Date());
                var sameDate = parent.dateIsEqual(d.t, new Date());

                if (parent.legendScale === null ||
                    (d.v === null && (options.hasOwnProperty("considerMissingDataAsZero") && !options.considerMissingDataAsZero) && !options.legendColors.hasOwnProperty("base"))
                ) {
                    htmlClass.push("graph-rect");
                }

                if (sameDate) {
                    htmlClass.push("now");
                } else if (!pastDate) {
                    htmlClass.push("future");
                }

                if (d.v !== null) {
                    htmlClass.push(parent.Legend.getClass(d.v, (parent.legendScale === null)));
                } else if (options.considerMissingDataAsZero && pastDate) {
                    htmlClass.push(parent.Legend.getClass(0, (parent.legendScale === null)));
                }

                if (options.onClick !== null) {
                    htmlClass.push("hover_cursor");
                }

                return htmlClass.join(" ");
            })
            .call(addStyle);

        rect.transition().duration(options.animationDuration).select("title")
            .text(function(d) {
                return parent.getSubDomainTitle(d);
            });

        function formatSubDomainText(element) {
            if (typeof options.subDomainTextFormat === "function") {
                element.text(function(d) {
                    return options.subDomainTextFormat(d.t, d.v);
                });
            }
        }

        /**
         * Change the subDomainText class if necessary
         * Also change the text, e.g when text is representing the value
         * instead of the date
         */
        rect.transition().duration(options.animationDuration).select("text")
            .attr("class", function(d) {
                return "subdomain-text" + parent.getHighlightClassName(d.t);
            })
            .call(formatSubDomainText);
    }

    /**
     * Sprintf like function.
     * Replaces placeholders {0} in string with values from provided object.
     * 
     * @param string formatted String containing placeholders.
     * @param object args Object with properties to replace placeholders in string.
     * 
     * @return String
     */
    formatStringWithObject(formatted, args) {
        "use strict";
        for (var prop in args) {
            if (args.hasOwnProperty(prop)) {
                var regexp = new RegExp("\\{" + prop + "\\}", "gi");
                formatted = formatted.replace(regexp, args[prop]);
            }
        }
        return formatted;
    }

    // =========================================================================//
    // EVENTS CALLBACK															//
    // =========================================================================//

    /**
     * Helper method for triggering event callback
     *
     * @param  string	eventName       Name of the event to trigger
     * @param  array	successArgs     List of argument to pass to the callback
     * @param  boolean  skip			Whether to skip the event triggering
     * @return mixed	True when the triggering was skipped, false on error, else the callback function
     */
    triggerEvent(eventName, successArgs, skip) {
        "use strict";

        if ((arguments.length === 3 && skip) || this.options[eventName] === null) {
            return true;
        }

        if (typeof this.options[eventName] === "function") {
            if (typeof successArgs === "function") {
                successArgs = successArgs();
            }
            return this.options[eventName].apply(this, successArgs);
        } else {
            console.log("Provided callback for " + eventName + " is not a function.");
            return false;
        }
    }

    /**
     * Event triggered on a mouse click on a subDomain cell
     *
     * @param  Date		d		Date of the subdomain block
     * @param  int		itemNb	Number of items in that date
     */
    onClick(d, itemNb) {
        "use strict";

        return this.triggerEvent("onClick", [d, itemNb]);
    }

    /**
     * Event triggered after drawing the calendar, byt before filling it with data
     */
    afterLoad() {
        "use strict";

        return this.triggerEvent("afterLoad");
    }

    /**
     * Event triggered after completing drawing and filling the calendar
     */
    onComplete() {
        "use strict";

        var response = this.triggerEvent("onComplete", [], this._completed);
        this._completed = true;
        return response;
    }

    /**
     * Event triggered after shifting the calendar one domain back
     *
     * @param  Date		start	Domain start date
     * @param  Date		end		Domain end date
     */
    afterLoadPreviousDomain(start) {
        "use strict";

        var parent = this;
        return this.triggerEvent("afterLoadPreviousDomain", function() {
            var subDomain = parent.getSubDomain(start);
            return [subDomain.shift(), subDomain.pop()];
        });
    }

    /**
     * Event triggered after shifting the calendar one domain above
     *
     * @param  Date		start	Domain start date
     * @param  Date		end		Domain end date
     */
    afterLoadNextDomain(start) {
        "use strict";

        var parent = this;
        return this.triggerEvent("afterLoadNextDomain", function() {
            var subDomain = parent.getSubDomain(start);
            return [subDomain.shift(), subDomain.pop()];
        });
    }

    /**
     * Event triggered after loading the leftmost domain allowed by minDate
     *
     * @param  boolean  reached True if the leftmost domain was reached
     */
    onMinDomainReached(reached) {
        "use strict";

        this._minDomainReached = reached;
        return this.triggerEvent("onMinDomainReached", [reached]);
    }

    /**
     * Event triggered after loading the rightmost domain allowed by maxDate
     *
     * @param  boolean  reached True if the rightmost domain was reached
     */
    onMaxDomainReached(reached) {
        "use strict";

        this._maxDomainReached = reached;
        return this.triggerEvent("onMaxDomainReached", [reached]);
    }

    checkIfMinDomainIsReached(date, upperBound) {
        "use strict";

        if (this.minDomainIsReached(date)) {
            this.onMinDomainReached(true);
        }

        if (arguments.length === 2) {
            if (this._maxDomainReached && !this.maxDomainIsReached(upperBound)) {
                this.onMaxDomainReached(false);
            }
        }
    }

    checkIfMaxDomainIsReached(date, lowerBound) {
        "use strict";

        if (this.maxDomainIsReached(date)) {
            this.onMaxDomainReached(true);
        }

        if (arguments.length === 2) {
            if (this._minDomainReached && !this.minDomainIsReached(lowerBound)) {
                this.onMinDomainReached(false);
            }
        }
    }

    // =========================================================================//
    // FORMATTER																//
    // =========================================================================//

    formatNumber() {
        return format(",g").apply(this, arguments);
    }

    formatDate(d, format$$1) {
        "use strict";

        if (arguments.length < 2) {
            format$$1 = "title";
        }

        if (typeof format$$1 === "function") {
            return format$$1(d);
        } else {
            var f = timeFormat(format$$1);
            return f(d);
        }
    }

    getSubDomainTitle(d) {
        "use strict";

        if (d.v === null && !this.options.considerMissingDataAsZero) {
            return this.formatStringWithObject(this.options.subDomainTitleFormat.empty, {
                date: this.formatDate(new Date(d.t), this.options.subDomainDateFormat)
            });
        } else {
            var value = d.v;
            // Consider null as 0
            if (value === null && this.options.considerMissingDataAsZero) {
                value = 0;
            }

            return this.formatStringWithObject(this.options.subDomainTitleFormat.filled, {
                count: this.formatNumber(value),
                name: this.options.itemName[(value !== 1 ? 1 : 0)],
                connector: this._domainType[this.options.subDomain].format.connector,
                date: this.formatDate(new Date(d.t), this.options.subDomainDateFormat)
            });
        }
    }

    // =========================================================================//
    // DOMAIN NAVIGATION														//
    // =========================================================================//

    /**
     * Shift the calendar one domain forward
     *
     * The new domain is loaded only if it's not beyond maxDate
     *
     * @param int n Number of domains to load
     * @return bool True if the next domain was loaded, else false
     */
    loadNextDomain(n) {
        "use strict";

        if (this._maxDomainReached || n === 0) {
            return false;
        }

        var bound = this.loadNewDomains(this.NAVIGATE_RIGHT, this.getDomain(this.getNextDomain(), n));

        this.afterLoadNextDomain(bound.end);
        this.checkIfMaxDomainIsReached(this.getNextDomain().getTime(), bound.start);

        return true;
    }

    /**
     * Shift the calendar one domain backward
     *
     * The previous domain is loaded only if it's not beyond the minDate
     *
     * @param int n Number of domains to load
     * @return bool True if the previous domain was loaded, else false
     */
    loadPreviousDomain(n) {
        "use strict";

        if (this._minDomainReached || n === 0) {
            return false;
        }

        var bound = this.loadNewDomains(this.NAVIGATE_LEFT, this.getDomain(this.getDomainKeys()[0], -n).reverse());

        this.afterLoadPreviousDomain(bound.start);
        this.checkIfMinDomainIsReached(bound.start, bound.end);

        return true;
    }

    loadNewDomains(direction, newDomains) {
        "use strict";

        var parent = this;
        var backward = direction === this.NAVIGATE_LEFT;
        var i = -1;
        var total = newDomains.length;
        var domains = this.getDomainKeys();

        function buildSubDomain(d) {
            return { t: parent._domainType[parent.options.subDomain].extractUnit(d), v: null };
        }

        // Remove out of bound domains from list of new domains to prepend
        while (++i < total) {
            if (backward && this.minDomainIsReached(newDomains[i])) {
                newDomains = newDomains.slice(0, i + 1);
                break;
            }
            if (!backward && this.maxDomainIsReached(newDomains[i])) {
                newDomains = newDomains.slice(0, i);
                break;
            }
        }

        newDomains = newDomains.slice(-this.options.range);

        for (i = 0, total = newDomains.length; i < total; i++) {
            this._domains.set(
                newDomains[i].getTime(),
                this.getSubDomain(newDomains[i]).map(buildSubDomain)
            );

            this._domains.remove(backward ? domains.pop() : domains.shift());
        }

        domains = this.getDomainKeys();

        if (backward) {
            newDomains = newDomains.reverse();
        }

        this.paint(direction);

        this.getDatas(
            this.options.data,
            newDomains[0],
            this.getSubDomain(newDomains[newDomains.length - 1]).pop(),
            function() {
                parent.fill(parent.lastInsertedSvg);
            }
        );

        return {
            start: newDomains[backward ? 0 : 1],
            end: domains[domains.length - 1]
        };
    }

    /**
     * Return whether a date is inside the scope determined by maxDate
     *
     * @param int datetimestamp The timestamp in ms to test
     * @return bool True if the specified date correspond to the calendar upper bound
     */
    maxDomainIsReached(datetimestamp) {
        "use strict";

        return (this.options.maxDate !== null && (this.options.maxDate.getTime() < datetimestamp));
    }

    /**
     * Return whether a date is inside the scope determined by minDate
     *
     * @param int datetimestamp The timestamp in ms to test
     * @return bool True if the specified date correspond to the calendar lower bound
     */
    minDomainIsReached(datetimestamp) {
        "use strict";

        return (this.options.minDate !== null && (this.options.minDate.getTime() >= datetimestamp));
    }

    /**
     * Return the list of the calendar's domain timestamp
     *
     * @return Array a sorted array of timestamp
     */
    getDomainKeys() {
        "use strict";

        return this._domains.keys()
            .map(function(d) {
                return parseInt(d, 10);
            })
            .sort(function(a, b) {
                return a - b;
            });
    }

    // =========================================================================//
    // POSITIONNING																//
    // =========================================================================//

    positionSubDomainX(d) {
        "use strict";

        var index = this._domainType[this.options.subDomain].position.x(new Date(d));
        return index * this.options.cellSize + index * this.options.cellPadding;
    }

    positionSubDomainY(d) {
        "use strict";

        var index = this._domainType[this.options.subDomain].position.y(new Date(d));
        return index * this.options.cellSize + index * this.options.cellPadding;
    }

    getSubDomainColumnNumber(d) {
        "use strict";

        if (this.options.rowLimit > 0) {
            var i = this._domainType[this.options.subDomain].maxItemNumber;
            if (typeof i === "function") {
                i = i(d);
            }
            return Math.ceil(i / this.options.rowLimit);
        }

        var j = this._domainType[this.options.subDomain].defaultColumnNumber;
        if (typeof j === "function") {
            j = j(d);

        }
        return this.options.colLimit || j;
    }

    getSubDomainRowNumber(d) {
        "use strict";

        if (this.options.colLimit > 0) {
            var i = this._domainType[this.options.subDomain].maxItemNumber;
            if (typeof i === "function") {
                i = i(d);
            }
            return Math.ceil(i / this.options.colLimit);
        }

        var j = this._domainType[this.options.subDomain].defaultRowNumber;
        if (typeof j === "function") {
            j = j(d);

        }
        return this.options.rowLimit || j;
    }

    /**
     * Return a classname if the specified date should be highlighted
     *
     * @param  timestamp date Date of the current subDomain
     * @return String the highlight class
     */
    getHighlightClassName(d) {
        "use strict";

        d = new Date(d);

        if (this.options.highlight.length > 0) {
            for (var i in this.options.highlight) {
                if (this.dateIsEqual(this.options.highlight[i], d)) {
                    return this.isNow(this.options.highlight[i]) ? " highlight-now" : " highlight";
                }
            }
        }
        return "";
    }

    /**
     * Return whether the specified date is now,
     * according to the type of subdomain
     *
     * @param  Date d The date to compare
     * @return bool True if the date correspond to a subdomain cell
     */
    isNow(d) {
        "use strict";

        return this.dateIsEqual(d, new Date());
    }

    /**
     * Return whether 2 dates are equals
     * This function is subdomain-aware,
     * and dates comparison are dependent of the subdomain
     *
     * @param  Date dateA First date to compare
     * @param  Date dateB Secon date to compare
     * @return bool true if the 2 dates are equals
     */
    /* jshint maxcomplexity: false */
    dateIsEqual(dateA, dateB) {
        "use strict";

        if (!(dateA instanceof Date)) {
            dateA = new Date(dateA);
        }

        if (!(dateB instanceof Date)) {
            dateB = new Date(dateB);
        }

        switch (this.options.subDomain) {
            case "x_min":
            case "min":
                return dateA.getFullYear() === dateB.getFullYear() &&
                    dateA.getMonth() === dateB.getMonth() &&
                    dateA.getDate() === dateB.getDate() &&
                    dateA.getHours() === dateB.getHours() &&
                    dateA.getMinutes() === dateB.getMinutes();
            case "x_hour":
            case "hour":
                return dateA.getFullYear() === dateB.getFullYear() &&
                    dateA.getMonth() === dateB.getMonth() &&
                    dateA.getDate() === dateB.getDate() &&
                    dateA.getHours() === dateB.getHours();
            case "x_day":
            case "day":
                return dateA.getFullYear() === dateB.getFullYear() &&
                    dateA.getMonth() === dateB.getMonth() &&
                    dateA.getDate() === dateB.getDate();
            case "x_week":
            case "week":
                return dateA.getFullYear() === dateB.getFullYear() &&
                    this.getWeekNumber(dateA) === this.getWeekNumber(dateB);
            case "x_month":
            case "month":
                return dateA.getFullYear() === dateB.getFullYear() &&
                    dateA.getMonth() === dateB.getMonth();
            default:
                return false;
        }
    }


    /**
     * Returns wether or not dateA is less than or equal to dateB. This function is subdomain aware.
     * Performs automatic conversion of values.
     * @param dateA may be a number or a Date
     * @param dateB may be a number or a Date
     * @returns {boolean}
     */
    dateIsLessThan(dateA, dateB) {
        "use strict";

        if (!(dateA instanceof Date)) {
            dateA = new Date(dateA);
        }

        if (!(dateB instanceof Date)) {
            dateB = new Date(dateB);
        }


        function normalizedMillis(date, subdomain) {
            switch (subdomain) {
                case "x_min":
                case "min":
                    return new Date(date.getFullYear(), date.getMonth(), date.getDate(), date.getHours(), date.getMinutes()).getTime();
                case "x_hour":
                case "hour":
                    return new Date(date.getFullYear(), date.getMonth(), date.getDate(), date.getHours()).getTime();
                case "x_day":
                case "day":
                    return new Date(date.getFullYear(), date.getMonth(), date.getDate()).getTime();
                case "x_week":
                case "week":
                case "x_month":
                case "month":
                    return new Date(date.getFullYear(), date.getMonth()).getTime();
                default:
                    return date.getTime();
            }
        }

        return normalizedMillis(dateA, this.options.subDomain) < normalizedMillis(dateB, this.options.subDomain);
    }


    // =========================================================================//
    // DATE COMPUTATION															//
    // =========================================================================//

    /**
     * Return the day of the year for the date
     * @param	Date
     * @return  int Day of the year [1,366]
     */
    getDayOfYear() {
        return timeFormat("%j").apply(this, arguments);
    }

    /**
     * Return the week number of the year
     * Monday as the first day of the week
     * @return int	Week number [0-53]
     */
    getWeekNumber(d) {
        "use strict";

        var f = this.options.weekStartOnMonday === true ? timeFormat("%W") : timeFormat("%U");
        return f(d);
    }

    /**
     * Return the week number, relative to its month
     *
     * @param  int|Date d Date or timestamp in milliseconds
     * @return int Week number, relative to the month [0-5]
     */
    getMonthWeekNumber(d) {
        "use strict";

        if (typeof d === "number") {
            d = new Date(d);
        }

        var monthFirstWeekNumber = this.getWeekNumber(new Date(d.getFullYear(), d.getMonth()));
        return this.getWeekNumber(d) - monthFirstWeekNumber - 1;
    }

    /**
     * Return the number of weeks in the dates' year
     *
     * @param  int|Date d Date or timestamp in milliseconds
     * @return int Number of weeks in the date's year
     */
    getWeekNumberInYear(d) {
        "use strict";

        if (typeof d === "number") {
            d = new Date(d);
        }
    }

    /**
     * Return the number of days in the date's month
     *
     * @param  int|Date d Date or timestamp in milliseconds
     * @return int Number of days in the date's month
     */
    getDayCountInMonth(d) {
        "use strict";

        return this.getEndOfMonth(d).getDate();
    }

    /**
     * Return the number of days in the date's year
     *
     * @param  int|Date d Date or timestamp in milliseconds
     * @return int Number of days in the date's year
     */
    getDayCountInYear(d) {
        "use strict";

        if (typeof d === "number") {
            d = new Date(d);
        }
        return (new Date(d.getFullYear(), 1, 29).getMonth() === 1) ? 366 : 365;
    }

    /**
     * Get the weekday from a date
     *
     * Return the week day number (0-6) of a date,
     * depending on whether the week start on monday or sunday
     *
     * @param  Date d
     * @return int The week day number (0-6)
     */
    getWeekDay(d) {
        "use strict";

        if (this.options.weekStartOnMonday === false) {
            return d.getDay();
        }
        return d.getDay() === 0 ? 6 : (d.getDay() - 1);
    }

    /**
     * Get the last day of the month
     * @param  Date|int	d	Date or timestamp in milliseconds
     * @return Date			Last day of the month
     */
    getEndOfMonth(d) {
        "use strict";

        if (typeof d === "number") {
            d = new Date(d);
        }
        return new Date(d.getFullYear(), d.getMonth() + 1, 0);
    }

    /**
     *
     * @param  Date date
     * @param  int count
     * @param  string step
     * @return Date
     */
    jumpDate(date, count, step) {
        "use strict";

        var d = new Date(date);
        switch (step) {
            case "hour":
                d.setHours(d.getHours() + count);
                break;
            case "day":
                d.setHours(d.getHours() + count * 24);
                break;
            case "week":
                d.setHours(d.getHours() + count * 24 * 7);
                break;
            case "month":
                d.setMonth(d.getMonth() + count);
                break;
            case "year":
                d.setFullYear(d.getFullYear() + count);
        }

        return new Date(d);
    }

    // =========================================================================//
    // DOMAIN COMPUTATION														//
    // =========================================================================//

    /**
     * Return all the minutes between 2 dates
     *
     * @param  Date	d	date	A date
     * @param  int|date	range	Number of minutes in the range, or a stop date
     * @return array	An array of minutes
     */
    getMinuteDomain(d, range$$1) {
        "use strict";

        var start = new Date(d.getFullYear(), d.getMonth(), d.getDate(), d.getHours());
        var stop = null;
        if (range$$1 instanceof Date) {
            stop = new Date(range$$1.getFullYear(), range$$1.getMonth(), range$$1.getDate(), range$$1.getHours());
        } else {
            stop = new Date(+start + range$$1 * 1000 * 60);
        }
        return minutes(Math.min(start, stop), Math.max(start, stop));
    }

    /**
     * Return all the hours between 2 dates
     *
     * @param  Date	d	A date
     * @param  int|date	range	Number of hours in the range, or a stop date
     * @return array	An array of hours
     */
    getHourDomain(d, range$$1) {
        "use strict";

        var start = new Date(d.getFullYear(), d.getMonth(), d.getDate(), d.getHours());
        var stop = null;
        if (range$$1 instanceof Date) {
            stop = new Date(range$$1.getFullYear(), range$$1.getMonth(), range$$1.getDate(), range$$1.getHours());
        } else {
            stop = new Date(start);
            stop.setHours(stop.getHours() + range$$1);
        }

        var domains = hours(Math.min(start, stop), Math.max(start, stop));

        // Passing from DST to standard time
        // If there are 25 hours, let's compress the duplicate hours
        var i = 0;
        var total = domains.length;
        for (i = 0; i < total; i++) {
            if (i > 0 && (domains[i].getHours() === domains[i - 1].getHours())) {
                this.DSTDomain.push(domains[i].getTime());
                domains.splice(i, 1);
                break;
            }
        }

        // d3.time.hours is returning more hours than needed when changing
        // from DST to standard time, because there is really 2 hours between
        // 1am and 2am!
        if (typeof range$$1 === "number" && domains.length > Math.abs(range$$1)) {
            domains.splice(domains.length - 1, 1);
        }

        return domains;
    }

    /**
     * Return all the days between 2 dates
     *
     * @param  Date		d		A date
     * @param  int|date	range	Number of days in the range, or a stop date
     * @return array	An array of weeks
     */
    getDayDomain(d, range$$1) {
        "use strict";

        var start = new Date(d.getFullYear(), d.getMonth(), d.getDate());
        var stop = null;
        if (range$$1 instanceof Date) {
            stop = new Date(range$$1.getFullYear(), range$$1.getMonth(), range$$1.getDate());
        } else {
            stop = new Date(start);
            stop = new Date(stop.setDate(stop.getDate() + parseInt(range$$1, 10)));
        }

        return days(Math.min(start, stop), Math.max(start, stop));
    }

    /**
     * Return all the weeks between 2 dates
     *
     * @param  Date	d	A date
     * @param  int|date	range	Number of minutes in the range, or a stop date
     * @return array	An array of weeks
     */
    getWeekDomain(d, range$$1) {
        "use strict";

        var weekStart;

        if (this.options.weekStartOnMonday === false) {
            weekStart = new Date(d.getFullYear(), d.getMonth(), d.getDate() - d.getDay());
        } else {
            if (d.getDay() === 1) {
                weekStart = new Date(d.getFullYear(), d.getMonth(), d.getDate());
            } else if (d.getDay() === 0) {
                weekStart = new Date(d.getFullYear(), d.getMonth(), d.getDate());
                weekStart.setDate(weekStart.getDate() - 6);
            } else {
                weekStart = new Date(d.getFullYear(), d.getMonth(), d.getDate() - d.getDay() + 1);
            }
        }

        var endDate = new Date(weekStart);

        var stop = range$$1;
        if (typeof range$$1 !== "object") {
            stop = new Date(endDate.setDate(endDate.getDate() + range$$1 * 7));
        }

        return (this.options.weekStartOnMonday === true) ?
            mondays(Math.min(weekStart, stop), Math.max(weekStart, stop)) :
            sundays(Math.min(weekStart, stop), Math.max(weekStart, stop));
    }

    /**
     * Return all the months between 2 dates
     *
     * @param  Date		d		A date
     * @param  int|date	range	Number of months in the range, or a stop date
     * @return array	An array of months
     */
    getMonthDomain(d, range$$1) {
        "use strict";

        var start = new Date(d.getFullYear(), d.getMonth());
        var stop = null;
        if (range$$1 instanceof Date) {
            stop = new Date(range$$1.getFullYear(), range$$1.getMonth());
        } else {
            stop = new Date(start);
            stop = stop.setMonth(stop.getMonth() + range$$1);
        }

        return months(Math.min(start, stop), Math.max(start, stop));
    }

    /**
     * Return all the years between 2 dates
     *
     * @param  Date	d	date	A date
     * @param  int|date	range	Number of minutes in the range, or a stop date
     * @return array	An array of hours
     */
    getYearDomain(d, range$$1) {
        "use strict";

        var start = new Date(d.getFullYear(), 0);
        var stop = null;
        if (range$$1 instanceof Date) {
            stop = new Date(range$$1.getFullYear(), 0);
        } else {
            stop = new Date(d.getFullYear() + range$$1, 0);
        }

        return years(Math.min(start, stop), Math.max(start, stop));
    }

    /**
     * Get an array of domain start dates
     *
     * @param  int|Date date A random date included in the wanted domain
     * @param  int|Date range Number of dates to get, or a stop date
     * @return Array of dates
     */
    getDomain(date, range$$1) {
        "use strict";

        if (typeof date === "number") {
            date = new Date(date);
        }

        if (arguments.length < 2) {
            range$$1 = this.options.range;
        }

        switch (this.options.domain) {
            case "hour":
                var domains = this.getHourDomain(date, range$$1);

                // Case where an hour is missing, when passing from standard time to DST
                // Missing hour is perfectly acceptabl in subDomain, but not in domains
                if (typeof range$$1 === "number" && domains.length < range$$1) {
                    if (range$$1 > 0) {
                        domains.push(this.getHourDomain(domains[domains.length - 1], 2)[1]);
                    } else {
                        domains.shift(this.getHourDomain(domains[0], -2)[0]);
                    }
                }
                return domains;
            case "day":
                return this.getDayDomain(date, range$$1);
            case "week":
                return this.getWeekDomain(date, range$$1);
            case "month":
                return this.getMonthDomain(date, range$$1);
            case "year":
                return this.getYearDomain(date, range$$1);
        }
    }

    /* jshint maxcomplexity: false */
    getSubDomain(date) {
        "use strict";

        if (typeof date === "number") {
            date = new Date(date);
        }

        var parent = this;

        /**
         * @return int
         */
        var computeDaySubDomainSize = function(date, domain) {
            switch (domain) {
                case "year":
                    return parent.getDayCountInYear(date);
                case "month":
                    return parent.getDayCountInMonth(date);
                case "week":
                    return 7;
            }
        };

        /**
         * @return int
         */
        var computeMinSubDomainSize = function(date, domain) {
            switch (domain) {
                case "hour":
                    return 60;
                case "day":
                    return 60 * 24;
                case "week":
                    return 60 * 24 * 7;
            }
        };

        /**
         * @return int
         */
        var computeHourSubDomainSize = function(date, domain) {
            switch (domain) {
                case "day":
                    return 24;
                case "week":
                    return 168;
                case "month":
                    return parent.getDayCountInMonth(date) * 24;
            }
        };

        /**
         * @return int
         */
        var computeWeekSubDomainSize = function(date, domain) {
            if (domain === "month") {
                var endOfMonth = new Date(date.getFullYear(), date.getMonth() + 1, 0);
                var endWeekNb = parent.getWeekNumber(endOfMonth);
                var startWeekNb = parent.getWeekNumber(new Date(date.getFullYear(), date.getMonth()));

                if (startWeekNb > endWeekNb) {
                    startWeekNb = 0;
                    endWeekNb++;
                }

                return endWeekNb - startWeekNb + 1;
            } else if (domain === "year") {
                return parent.getWeekNumber(new Date(date.getFullYear(), 11, 31));
            }
        };

        switch (this.options.subDomain) {
            case "x_min":
            case "min":
                return this.getMinuteDomain(date, computeMinSubDomainSize(date, this.options.domain));
            case "x_hour":
            case "hour":
                return this.getHourDomain(date, computeHourSubDomainSize(date, this.options.domain));
            case "x_day":
            case "day":
                return this.getDayDomain(date, computeDaySubDomainSize(date, this.options.domain));
            case "x_week":
            case "week":
                return this.getWeekDomain(date, computeWeekSubDomainSize(date, this.options.domain));
            case "x_month":
            case "month":
                return this.getMonthDomain(date, 12);
        }
    }

    /**
     * Get the n-th next domain after the calendar newest (rightmost) domain
     * @param  int n
     * @return Date The start date of the wanted domain
     */
    getNextDomain(n) {
        "use strict";

        if (arguments.length === 0) {
            n = 1;
        }
        return this.getDomain(this.jumpDate(this.getDomainKeys().pop(), n, this.options.domain), 1)[0];
    }

    /**
     * Get the n-th domain before the calendar oldest (leftmost) domain
     * @param  int n
     * @return Date The start date of the wanted domain
     */
    getPreviousDomain(n) {
        "use strict";

        if (arguments.length === 0) {
            n = 1;
        }
        return this.getDomain(this.jumpDate(this.getDomainKeys().shift(), -n, this.options.domain), 1)[0];
    }


    // =========================================================================//
    // DATAS																	//
    // =========================================================================//

    /**
     * Fetch and interpret data from the datasource
     *
     * @param string|object source
     * @param Date startDate
     * @param Date endDate
     * @param function callback
     * @param function|boolean afterLoad function used to convert the data into a json object. Use true to use the afterLoad callback
     * @param updateMode
     *
     * @return mixed
     * - True if there are no data to load
     * - False if data are loaded asynchronously
     */
    getDatas(source, startDate, endDate, callback, afterLoad, updateMode) {
        "use strict";

        var self = this;
        if (arguments.length < 5) {
            afterLoad = true;
        }
        if (arguments.length < 6) {
            updateMode = this.APPEND_ON_UPDATE;
        }
        var _callback = function(data) {
            if (afterLoad !== false) {
                if (typeof afterLoad === "function") {
                    data = afterLoad(data);
                } else if (typeof(self.options.afterLoadData) === "function") {
                    data = self.options.afterLoadData(data);
                } else {
                    console.log("Provided callback for afterLoadData is not a function.");
                }
            } else if (self.options.dataType === "csv" || self.options.dataType === "tsv") {
                data = this.interpretCSV(data);
            }
            self.parseDatas(data, updateMode, startDate, endDate);
            if (typeof callback === "function") {
                callback();
            }
        };

        switch (typeof source) {
            case "string":
                if (source === "") {
                    _callback({});
                    return true;
                } else {
                    var url = this.parseURI(source, startDate, endDate);
                    var requestType = "GET";
                    if (self.options.dataPostPayload !== null) {
                        requestType = "POST";
                    }
                    var payload = null;
                    if (self.options.dataPostPayload !== null) {
                        payload = this.parseURI(self.options.dataPostPayload, startDate, endDate);
                    }

                    switch (this.options.dataType) {
                        case "json":
                            json(url, _callback).send(requestType, payload);
                            break;
                        case "csv":
                            csv(url, _callback).send(requestType, payload);
                            break;
                        case "tsv":
                            tsv$1(url, _callback).send(requestType, payload);
                            break;
                        case "txt":
                            text(url, _callback).send(requestType, payload);
                            break;
                    }
                }
                return false;
            case "object":
                if (source === Object(source)) {
                    _callback(source);
                    return false;
                }
                /* falls through */
            default:
                _callback({});
                return true;
        }
    }

    /**
     * Populate the calendar internal data
     *
     * @param object data
     * @param constant updateMode
     * @param Date startDate
     * @param Date endDate
     *
     * @return void
     */
    parseDatas(data, updateMode, startDate, endDate) {
        "use strict";

        if (updateMode === this.RESET_ALL_ON_UPDATE) {
            this._domains.forEach(function(key, value) {
                value.forEach(function(element, index, array) {
                    array[index].v = null;
                });
            });
        }

        var temp = {};

        var extractTime = function(d) {
            return d.t;
        };

        /*jshint forin:false */
        for (var d in data) {
            var date = new Date(d * 1000);
            var domainUnit = this.getDomain(date)[0].getTime();

            // The current data belongs to a domain that was compressed
            // Compress the data for the two duplicate hours into the same hour
            if (this.DSTDomain.indexOf(domainUnit) >= 0) {

                // Re-assign all data to the first or the second duplicate hours
                // depending on which is visible
                if (this._domains.has(domainUnit - 3600 * 1000)) {
                    domainUnit -= 3600 * 1000;
                }
            }

            // Skip if data is not relevant to current domain
            if (isNaN(d) || !data.hasOwnProperty(d) || !this._domains.has(domainUnit) || !(domainUnit >= +startDate && domainUnit < +endDate)) {
                continue;
            }

            var subDomainsData = this._domains.get(domainUnit);

            if (!temp.hasOwnProperty(domainUnit)) {
                temp[domainUnit] = subDomainsData.map(extractTime);
            }

            var index = temp[domainUnit].indexOf(this._domainType[this.options.subDomain].extractUnit(date));

            if (updateMode === this.RESET_SINGLE_ON_UPDATE) {
                subDomainsData[index].v = data[d];
            } else {
                if (!isNaN(subDomainsData[index].v)) {
                    subDomainsData[index].v += data[d];
                } else {
                    subDomainsData[index].v = data[d];
                }
            }
        }
    }

    parseURI(str, startDate, endDate) {
        "use strict";

        // Use a timestamp in seconds
        str = str.replace(/\{\{t:start\}\}/g, startDate.getTime() / 1000);
        str = str.replace(/\{\{t:end\}\}/g, endDate.getTime() / 1000);

        // Use a string date, following the ISO-8601
        str = str.replace(/\{\{d:start\}\}/g, startDate.toISOString());
        str = str.replace(/\{\{d:end\}\}/g, endDate.toISOString());

        return str;
    }

    interpretCSV(data) {
        "use strict";

        var d = {};
        var keys$$1 = Object.keys(data[0]);
        var i, total;
        for (i = 0, total = data.length; i < total; i++) {
            d[data[i][keys$$1[0]]] = +data[i][keys$$1[1]];
        }
        return d;
    }

    /**
     * Handle the calendar layout and dimension
     *
     * Expand and shrink the container depending on its children dimension
     * Also rearrange the children position depending on their dimension,
     * and the legend position
     *
     * @return void
     */
    resize() {
        "use strict";

        var parent = this;
        var options = parent.options;
        var legendWidth = options.displayLegend ? (parent.Legend.getDim("width") + options.legendMargin[1] + options.legendMargin[3]) : 0;
        var legendHeight = options.displayLegend ? (parent.Legend.getDim("height") + options.legendMargin[0] + options.legendMargin[2]) : 0;

        var graphWidth = parent.graphDim.width - options.domainGutter - options.cellPadding;
        var graphHeight = parent.graphDim.height - options.domainGutter - options.cellPadding;

        this.root.transition().duration(options.animationDuration)
            .attr("width", function() {
                if (options.legendVerticalPosition === "middle" || options.legendVerticalPosition === "center") {
                    return graphWidth + legendWidth;
                }
                return Math.max(graphWidth, legendWidth);
            })
            .attr("height", function() {
                if (options.legendVerticalPosition === "middle" || options.legendVerticalPosition === "center") {
                    return Math.max(graphHeight, legendHeight);
                }
                return graphHeight + legendHeight;
            });

        this.root.select(".graph").transition().duration(options.animationDuration)
            .attr("y", function() {
                if (options.legendVerticalPosition === "top") {
                    return legendHeight;
                }
                return 0;
            })
            .attr("x", function() {
                if (
                    (options.legendVerticalPosition === "middle" || options.legendVerticalPosition === "center") &&
                    options.legendHorizontalPosition === "left") {
                    return legendWidth;
                }
                return 0;

            });
    }

    // =========================================================================//
    // PUBLIC API																//
    // =========================================================================//

    /**
     * Shift the calendar forward
     */
    next(n) {
        "use strict";

        if (arguments.length === 0) {
            n = 1;
        }
        return this.loadNextDomain(n);
    }

    /**
     * Shift the calendar backward
     */
    previous(n) {
        "use strict";

        if (arguments.length === 0) {
            n = 1;
        }
        return this.loadPreviousDomain(n);
    }

    /**
     * Jump directly to a specific date
     *
     * JumpTo will scroll the calendar until the wanted domain with the specified
     * date is visible. Unless you set reset to true, the wanted domain
     * will not necessarily be the first (leftmost) domain of the calendar.
     *
     * @param Date date Jump to the domain containing that date
     * @param bool reset Whether the wanted domain should be the first domain of the calendar
     * @param bool True of the calendar was scrolled
     */
    jumpTo(date, reset) {
        "use strict";

        if (arguments.length < 2) {
            reset = false;
        }
        var domains = this.getDomainKeys();
        var firstDomain = domains[0];
        var lastDomain = domains[domains.length - 1];

        if (date < firstDomain) {
            return this.loadPreviousDomain(this.getDomain(firstDomain, date).length);
        } else {
            if (reset) {
                return this.loadNextDomain(this.getDomain(firstDomain, date).length);
            }

            if (date > lastDomain) {
                return this.loadNextDomain(this.getDomain(lastDomain, date).length);
            }
        }

        return false;
    }

    /**
     * Navigate back to the start date
     *
     * @since  3.3.8
     * @return void
     */
    rewind() {
        "use strict";

        this.jumpTo(this.options.start, true);
    }

    /**
     * Update the calendar with new data
     *
     * @param  object|string		dataSource		The calendar's datasource, same type as this.options.data
     * @param  boolean|function		afterLoad		Whether to execute afterLoad() on the data. Pass directly a function
     * if you don't want to use the afterLoad() callback
     */
    update(dataSource, afterLoad, updateMode) {
        "use strict";

        if (arguments.length < 2) {
            afterLoad = true;
        }
        if (arguments.length < 3) {
            updateMode = this.RESET_ALL_ON_UPDATE;
        }

        var domains = this.getDomainKeys();
        var self = this;
        this.getDatas(
            dataSource,
            new Date(domains[0]),
            this.getSubDomain(domains[domains.length - 1]).pop(),
            function() {
                self.fill();
            },
            afterLoad,
            updateMode
        );
    }

    /**
     * Set the legend
     *
     * @param array legend an array of integer, representing the different threshold value
     * @param array colorRange an array of 2 hex colors, for the minimum and maximum colors
     */
    setLegend() {
        "use strict";

        var oldLegend = this.options.legend.slice(0);
        if (arguments.length >= 1 && Array.isArray(arguments[0])) {
            this.options.legend = arguments[0];
        }
        if (arguments.length >= 2) {
            if (Array.isArray(arguments[1]) && arguments[1].length >= 2) {
                this.options.legendColors = [arguments[1][0], arguments[1][1]];
            } else {
                this.options.legendColors = arguments[1];
            }
        }

        if ((arguments.length > 0 && !arrayEquals(oldLegend, this.options.legend)) || arguments.length >= 2) {
            this.Legend.buildColors();
            this.fill();
        }

        this.Legend.redraw(this.graphDim.width - this.options.domainGutter - this.options.cellPadding);
    }

    /**
     * Remove the legend
     *
     * @return bool False if there is no legend to remove
     */
    removeLegend() {
        "use strict";

        if (!this.options.displayLegend) {
            return false;
        }
        this.options.displayLegend = false;
        this.Legend.remove();
        return true;
    }

    /**
     * Display the legend
     *
     * @return bool False if the legend was already displayed
     */
    showLegend() {
        "use strict";

        if (this.options.displayLegend) {
            return false;
        }
        this.options.displayLegend = true;
        this.Legend.redraw(this.graphDim.width - this.options.domainGutter - this.options.cellPadding);
        return true;
    }

    /**
     * Highlight dates
     *
     * Add a highlight class to a set of dates
     *
     * @since  3.3.5
     * @param  array Array of dates to highlight
     * @return bool True if dates were highlighted
     */
    highlight(args) {
        "use strict";

        if ((this.options.highlight = this.expandDateSetting(args)).length > 0) {
            this.fill();
            return true;
        }
        return false;
    }

    /**
     * Destroy the calendar
     *
     * Usage: cal = cal.destroy();
     *
     * @since  3.3.6
     * @param function A callback function to trigger after destroying the calendar
     * @return null
     */
    destroy(callback) {
        "use strict";

        this.root.transition().duration(this.options.animationDuration)
            .attr("width", 0)
            .attr("height", 0)
            .remove()
            .each(function() {
                if (typeof callback === "function") {
                    callback();
                } else if (typeof callback !== "undefined") {
                    console.log("Provided callback for destroy() is not a function.");
                }
            });

        return null;
    }

    getSVG() {
        "use strict";

        var styles = {
            ".cal-heatmap-container": {},
            ".graph": {},
            ".graph-rect": {},
            "rect.highlight": {},
            "rect.now": {},
            "rect.highlight-now": {},
            "text.highlight": {},
            "text.now": {},
            "text.highlight-now": {},
            ".domain-background": {},
            ".graph-label": {},
            ".subdomain-text": {},
            ".q0": {},
            ".qi": {}
        };

        for (var j = 1, total = this.options.legend.length + 1; j <= total; j++) {
            styles[".q" + j] = {};
        }

        var root = this.root;

        var whitelistStyles = [
            // SVG specific properties
            "stroke", "stroke-width", "stroke-opacity", "stroke-dasharray", "stroke-dashoffset", "stroke-linecap", "stroke-miterlimit",
            "fill", "fill-opacity", "fill-rule",
            "marker", "marker-start", "marker-mid", "marker-end",
            "alignement-baseline", "baseline-shift", "dominant-baseline", "glyph-orientation-horizontal", "glyph-orientation-vertical", "kerning", "text-anchor",
            "shape-rendering",

            // Text Specific properties
            "text-transform", "font-family", "font", "font-size", "font-weight"
        ];

        var filterStyles = function(attribute, property, value) {
            if (whitelistStyles.indexOf(property) !== -1) {
                styles[attribute][property] = value;
            }
        };

        var getElement = function(e) {
            return root.select(e).node();
        };

        /* jshint forin:false */
        for (var element in styles) {
            if (!styles.hasOwnProperty(element)) {
                continue;
            }

            var dom = getElement(element);

            if (dom === null) {
                continue;
            }

            // The DOM Level 2 CSS way
            /* jshint maxdepth: false */
            if ("getComputedStyle" in window) {
                var cs = getComputedStyle(dom, null);
                if (cs.length !== 0) {
                    for (var i = 0; i < cs.length; i++) {
                        filterStyles(element, cs.item(i), cs.getPropertyValue(cs.item(i)));
                    }

                    // Opera workaround. Opera doesn"t support `item`/`length`
                    // on CSSStyleDeclaration.
                } else {
                    for (var k in cs) {
                        if (cs.hasOwnProperty(k)) {
                            filterStyles(element, k, cs[k]);
                        }
                    }
                }

                // The IE way
            } else if ("currentStyle" in dom) {
                var css = dom.currentStyle;
                for (var p in css) {
                    filterStyles(element, p, css[p]);
                }
            }
        }

        var string = "<svg xmlns=\"http://www.w3.org/2000/svg\" " +
            "xmlns:xlink=\"http://www.w3.org/1999/xlink\"><style type=\"text/css\"><![CDATA[ ";

        for (var style in styles) {
            string += style + " {\n";
            for (var l in styles[style]) {
                string += "\t" + l + ":" + styles[style][l] + ";\n";
            }
            string += "}\n";
        }

        string += "]]></style>";
        string += new XMLSerializer().serializeToString(this.root.node());
        string += "</svg>";

        return string;
    }
}

// =========================================================================//
// DOMAIN POSITION COMPUTATION												//
// =========================================================================//

/**
 * Compute the position of a domain, relative to the calendar
 */
class DomainPosition {
    constructor() {
        "use strict";

        this.positions = map$1();
    }

    getPosition(d) {
        "use strict";

        return this.positions.get(d);
    }

    getPositionFromIndex(i) {
        "use strict";

        var domains = this.getKeys();
        return this.positions.get(domains[i]);
    }

    getLast() {
        "use strict";

        var domains = this.getKeys();
        return this.positions.get(domains[domains.length - 1]);
    }

    setPosition(d, dim) {
        "use strict";

        this.positions.set(d, dim);
    }

    shiftRightBy(exitingDomainDim) {
        "use strict";

        this.positions.each(function(key, value, map$$1) {
            map$$1.set(key, value - exitingDomainDim);
        });

        var domains = this.getKeys();
        this.positions.remove(domains[0]);
    }

    shiftLeftBy(enteringDomainDim) {
        "use strict";

        this.positions.each(function(key, value, map$$1) {
            map$$1.set(key, value + enteringDomainDim);
        });

        var domains = this.getKeys();
        this.positions.remove(domains[domains.length - 1]);
    }

    getKeys() {
        "use strict";

        return this.positions.keys().sort(function(a, b) {
            return parseInt(a, 10) - parseInt(b, 10);
        });
    }
}

// =========================================================================//
// LEGEND																	//
// =========================================================================//

class Legend {
    constructor(calendar) {
        "use strict";

        this.calendar = calendar;
        this.computeDim();

        if (calendar.options.legendColors !== null) {
            this.buildColors();
        }
    }

    computeDim() {
        "use strict";

        var options = this.calendar.options; // Shorter accessor for variable name mangling when minifying
        this.dim = {
            width: options.legendCellSize * (options.legend.length + 1) +
                options.legendCellPadding * (options.legend.length),
            height: options.legendCellSize
        };
    }

    remove() {
        "use strict";

        this.calendar.root.select(".graph-legend").remove();
        this.calendar.resize();
    }

    redraw(width) {
        "use strict";

        if (!this.calendar.options.displayLegend) {
            return false;
        }

        var parent = this;
        var calendar = this.calendar;
        var legend = calendar.root;
        var legendItem;
        var options = calendar.options; // Shorter accessor for variable name mangling when minifying

        this.computeDim();

        var _legend = options.legend.slice(0);
        _legend.push(_legend[_legend.length - 1] + 1);

        var legendElement = calendar.root.select(".graph-legend");
        if (legendElement.node() !== null) {
            legend = legendElement;
            legendItem = legend
                .select("g")
                .selectAll("rect").data(_legend);
        } else {
            // Creating the new legend DOM if it doesn't already exist
            legend = options.legendVerticalPosition === "top" ? legend.insert("svg", ".graph") : legend.append("svg");

            legend
                .attr("x", getLegendXPosition())
                .attr("y", getLegendYPosition());

            legendItem = legend
                .attr("class", "graph-legend")
                .attr("height", parent.getDim("height"))
                .attr("width", parent.getDim("width"))
                .append("g")
                .selectAll().data(_legend);
        }

        legendItem
            .enter()
            .append("rect")
            .call(legendCellLayout)
            .attr("class", function(d) {
                return calendar.Legend.getClass(d, (calendar.legendScale === null));
            })
            .attr("fill-opacity", 0)
            .call(function(selection$$1) {
                if (calendar.legendScale !== null && options.legendColors !== null && options.legendColors.hasOwnProperty("base")) {
                    selection$$1.attr("fill", options.legendColors.base);
                }
            })
            .append("title");

        legendItem.exit().transition().duration(options.animationDuration)
            .attr("fill-opacity", 0)
            .remove();

        legendItem.transition().delay(function(d, i) {
                return options.animationDuration * i / 10;
            })
            .call(legendCellLayout)
            .attr("fill-opacity", 1)
            .call(function(element) {
                element.attr("fill", function(d, i) {
                    if (calendar.legendScale === null) {
                        return "";
                    }

                    if (i === 0) {
                        return calendar.legendScale(d - 1);
                    }
                    return calendar.legendScale(options.legend[i - 1]);
                });

                element.attr("class", function(d) {
                    return calendar.Legend.getClass(d, (calendar.legendScale === null));
                });
            });

        function legendCellLayout(selection$$1) {
            selection$$1
                .attr("width", options.legendCellSize)
                .attr("height", options.legendCellSize)
                .attr("x", function(d, i) {
                    return i * (options.legendCellSize + options.legendCellPadding);
                });
        }

        legendItem.select("title").text(function(d, i) {
            if (i === 0) {
                return calendar.formatStringWithObject(options.legendTitleFormat.lower, {
                    min: options.legend[i],
                    name: options.itemName[1]
                });
            } else if (i === _legend.length - 1) {
                return calendar.formatStringWithObject(options.legendTitleFormat.upper, {
                    max: options.legend[i - 1],
                    name: options.itemName[1]
                });
            } else {
                return calendar.formatStringWithObject(options.legendTitleFormat.inner, {
                    down: options.legend[i - 1],
                    up: options.legend[i],
                    name: options.itemName[1]
                });
            }
        });

        legend.transition().duration(options.animationDuration)
            .attr("x", getLegendXPosition())
            .attr("y", getLegendYPosition())
            .attr("width", parent.getDim("width"))
            .attr("height", parent.getDim("height"));

        legend.select("g").transition().duration(options.animationDuration)
            .attr("transform", function() {
                if (options.legendOrientation === "vertical") {
                    return "rotate(90 " + options.legendCellSize / 2 + " " + options.legendCellSize / 2 + ")";
                }
                return "";
            });

        function getLegendXPosition() {
            switch (options.legendHorizontalPosition) {
                case "right":
                    if (options.legendVerticalPosition === "center" || options.legendVerticalPosition === "middle") {
                        return width + options.legendMargin[3];
                    }
                    return width - parent.getDim("width") - options.legendMargin[1];
                case "middle":
                case "center":
                    return Math.round(width / 2 - parent.getDim("width") / 2);
                default:
                    return options.legendMargin[3];
            }
        }

        function getLegendYPosition() {
            if (options.legendVerticalPosition === "bottom") {
                return calendar.graphDim.height + options.legendMargin[0] - options.domainGutter - options.cellPadding;
            }
            return options.legendMargin[0];
        }

        calendar.resize();
    }

    /**
     * Return the dimension of the legend
     *
     * Takes into account rotation
     *
     * @param  string axis Width or height
     * @return int height or width in pixels
     */
    getDim(axis) {
        "use strict";

        var isHorizontal = (this.calendar.options.legendOrientation === "horizontal");

        switch (axis) {
            case "width":
                return this.dim[isHorizontal ? "width" : "height"];
            case "height":
                return this.dim[isHorizontal ? "height" : "width"];
        }
    }

    buildColors() {
        "use strict";

        var options = this.calendar.options; // Shorter accessor for variable name mangling when minifying

        if (options.legendColors === null) {
            this.calendar.legendScale = null;
            return false;
        }

        var _colorRange = [];

        if (Array.isArray(options.legendColors)) {
            _colorRange = options.legendColors;
        } else if (options.legendColors.hasOwnProperty("min") && options.legendColors.hasOwnProperty("max")) {
            _colorRange = [options.legendColors.min, options.legendColors.max];
        } else {
            options.legendColors = null;
            return false;
        }

        var _legend = options.legend.slice(0);

        if (_legend[0] > 0) {
            _legend.unshift(0);
        } else if (_legend[0] < 0) {
            // Let's guess the leftmost value, it we have to add one
            _legend.unshift(_legend[0] - (_legend[_legend.length - 1] - _legend[0]) / _legend.length);
        }

        var colorScale = linear$1()
            .range(_colorRange)
            .interpolate(interpolateHcl)
            .domain([min(_legend), max(_legend)]);

        var legendColors = _legend.map(function(element) {
            return colorScale(element);
        });
        this.calendar.legendScale = threshold$1().domain(options.legend).range(legendColors);

        return true;
    }

    /**
     * Return the classname on the legend for the specified value
     *
     * @param integer n Value associated to a date
     * @param bool withCssClass Whether to display the css class used to style the cell.
     *                          Disabling will allow styling directly via html fill attribute
     *
     * @return string Classname according to the legend
     */
    getClass(n, withCssClass) {
        "use strict";

        if (n === null || isNaN(n)) {
            return "";
        }

        var index = [this.calendar.options.legend.length + 1];

        for (var i = 0, total = this.calendar.options.legend.length - 1; i <= total; i++) {

            if (this.calendar.options.legend[0] > 0 && n < 0) {
                index = ["1", "i"];
                break;
            }

            if (n <= this.calendar.options.legend[i]) {
                index = [i + 1];
                break;
            }
        }

        if (n === 0) {
            index.push(0);
        }

        index.unshift("");
        return (index.join(" r") + (withCssClass ? index.join(" q") : "")).trim();
    }
}

/**
 * #source http://stackoverflow.com/a/383245/805649
 */
function mergeRecursive(obj1, obj2) {
    "use strict";

    /*jshint forin:false */
    for (var p in obj2) {
        try {
            // Property in destination object set; update its value.
            if (obj2[p].constructor === Object) {
                obj1[p] = mergeRecursive(obj1[p], obj2[p]);
            } else {
                obj1[p] = obj2[p];
            }
        } catch (e) {
            // Property in destination object not set; create it and set its value.
            obj1[p] = obj2[p];
        }
    }

    return obj1;
}

/**
 * Check if 2 arrays are equals
 *
 * @link http://stackoverflow.com/a/14853974/805649
 * @param  array array the array to compare to
 * @return bool true of the 2 arrays are equals
 */
function arrayEquals(arrayA, arrayB) {
    "use strict";

    // if the other array is a falsy value, return
    if (!arrayB || !arrayA) {
        return false;
    }

    // compare lengths - can save a lot of time
    if (arrayA.length !== arrayB.length) {
        return false;
    }

    for (var i = 0; i < arrayA.length; i++) {
        // Check if we have nested arrays
        if (arrayA[i] instanceof Array && arrayB[i] instanceof Array) {
            // recurse into the nested arrays
            if (!arrayEquals(arrayA[i], arrayB[i])) {
                return false;
            }
        } else if (arrayA[i] !== arrayB[i]) {
            // Warning - two different object instances will never be equal: {x:20} != {x:20}
            return false;
        }
    }
    return true;
}

export default CalHeatMap;
//# sourceMappingURL=cal-heatmap.es2015.full.js.map
