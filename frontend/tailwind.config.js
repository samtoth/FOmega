module.exports = {
    purge: {
        enabled: true,
        content: [
            './src/**/*.elm'
        ]
    },
    darkMode: false, // or 'media' or 'class'
    theme: {
        extend: {
            'serif': ["Volkorn", "Palatino", 'ui-serif', 'Georgia'],
            typography: {
                    css: {
                        h1: {
                            textAlign: 'center'
                        },
                    }
            }
        }
    },
    variants: {
        extend: {},
    },
    plugins: [require('@tailwindcss/typography'),],
}

