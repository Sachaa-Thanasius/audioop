from setuptools import Extension, setup

try:
    from wheel.bdist_wheel import bdist_wheel
except ImportError:
    cmdclass = {}
else:

    class bdist_wheel_abi3(bdist_wheel):
        def get_tag(self) -> tuple[str, str, str]:
            python, abi, plat = super().get_tag()

            if python.startswith("cp"):
                # For other cases, maybe in CI, use the current CPython version:
                # import sys
                # return f"cp3{sys.version_info[1]}", "abi3", plat
                return "cp38", "abi3", plat

            return python, abi, plat

    cmdclass = {"bdist_wheel": bdist_wheel_abi3}


setup(
    hpy_ext_modules=[
        Extension(
            name="audioop",  # as it would be imported; may include packages/namespaces separated by `.`
            sources=["src/audioop.c"],  # all sources are compiled into a single binary file
            py_limited_api=True,
            define_macros=[("Py_LIMITED_API", None)],
        ),
    ],
    cmdclass=cmdclass,
)
