module.exports = {
  purge: [],
  darkMode: false, // or 'media' or 'class'
  theme: {
    fontFamily: {
      'karla' : ['Karla', 'sans-serif'],
      'facit' : ['jaf-facitweb', 'sans-serif'],
      'icon' : ['"Material Icons"']
    },
    borderRadius: {
      'none': '0',
      DEFAULT: '15px',
      'small' : '11px',
      'full': '9999px'
    },
    fontSize: {
      'h1': '2.5rem',
      'h2': '1.5625rem',
      'label': '.9375rem',
      'smol': '.9375rem',
      'icon': '1.5rem',
      'body': '1.0625rem'
    },
    extend: {
      width: {
        sm : '640px',
      },
      margin: {
        '0.3' : '0.125rem',
      },
      padding: {
        '2.5' : '0.625rem',
        '0.3' : '0.125rem',
        '0.5' : '0.1875rem',
        '3.5' : '0.875rem',
        '5': '1.25rem'
      },
      boxShadow: {
        'small': '0px 1px 2px rgba(0, 0, 0, 0.25)',
        'header': '0px 2px 4px rgba(0, 0, 0, 0.25)',
        'input': 'inset 0px 1px 2px rgba(0, 0, 0, 0.25)',
        'button': '0px 4px 8px rgba(0, 0, 0, 0.25)'
      },
      colors: {
        primary: {
          light: '#FFEFE5',
          DEFAULT: '#FF823C',
          dark: '#B3876D',
          darker: '#A77559',
          rich: '#FF6D31',
          desaturated: '#FF8B4A'
        },
        error: {
          DEFAULT: '#FF003D',
          inset: '#FAF2F1'
        },
        raised: '#FFFDFC',
        background: '#FAF7F5',
        sunken: '#F5F0ED',
        'less-sunken': '#FCF9F7',
        metaline: '#EBE0DA',
        inset: '#F2EFED',
        copy: '#33190a',
        label: '#806759',
        light: '#998F8A',
        link: '#978880',
      },
    },
  },
  variants: {
    extend: {
      backgroundColor: ['active'],
      opacity: ['group-focus'],
    },
  },
  plugins: [],
}
