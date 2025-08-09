FROM python:3.11
WORKDIR /app
COPY ../requirements-all.txt ../requirements.txt ../requirements-dev.txt /app
RUN pip install --upgrade pip wheel setuptools \
    && if [ -f requirements-all.txt ]; then \
        pip install -r requirements-all.txt; \
    elif [ -f requirements.txt ]; then \
        pip install -r requirements.txt; \
    fi \
    && pip install -e . \
    && if [ -f requirements-dev.txt ]; then \
        pip install -r requirements-dev.txt; \
    else \
        echo "Missing requirements-dev.txt. Installing minimal requirements for testing." \
        pip install pytest pytest-cov pytest-xdist pytest-check aiohttp nbconvert jupyter_contrib_nbextensions Pygments respx markdown beautifulsoup4 Pillow async-cache lxml; \
    fi \
    && pip install "pandas>=1.3.0" "pygeohash>=1.2.0" "xgboost"
COPY ../ /app
RUN mkdir -p ~/.msticpy/mordor \
    && cp ./tests/testdata/geolite/GeoLite2-City.mmdb ~/.msticpy \
    && touch ~/.msticpy/GeoLite2-City.mmdb \
    && cp -r ./tests/testdata/mordor/* ~/.msticpy/mordor \
    && touch ~/.msticpy/mordor/mitre_tact_cache.pkl \
    && touch ~/.msticpy/mordor/mitre_tech_cache.pkl \
    && touch ~/.msticpy/mordor/mordor_cache.pkl
CMD ["pytest", "tests", "-n", "auto", "--cov=msticpy", "--cov-report=xml"]