module.exports = {
    plugins: {
        tailwindcss: {},
        autoprefixer: {},
        cssnano: {
            preset: [
                'default',
                {
                    discardComments: {
                        removeAll: true
                    }
                }
            ]
        },
        ...(process.env.NODE_ENV === "production" ? {
            "postcss-elm-tailwind": {
                tailwindConfig: "./tailwind.config.js",
                elmFile: "tailwind/TLWND.elm",
                elmModuleName: "TLWND",
                formats: {
                    svg: {
                        elmFile: "tailwind/TLWND/Svg.elm",
                        elmModuleName: "TLWND.Svg"
                    },
                    string: {
                        elmFile: "tailwind/TLWND/String.elm",
                        elmModuleName: "TLWND.String"
                    }
                }
            }

        } : {
        })
    }
};
