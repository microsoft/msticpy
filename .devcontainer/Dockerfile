# See here for image contents: https://hub.docker.com/r/jupyter/scipy-notebook
FROM jupyter/scipy-notebook
LABEL maintainer="Msticpy Dev Team <msticpy@microsoft.com>"

#installing Msticpy requirements and dependencies
COPY ./requirements-all.txt /tmp/
COPY ./requirements-dev.txt /tmp/
RUN pip install --requirement /tmp/requirements-all.txt & \
pip install --requirement /tmp/requirements-dev.txt & \
pip install -e git+https://github.com/microsoft/msticpy
COPY . /tmp/

# Activate ipywidgets extension in the environment that runs the notebook server
RUN jupyter nbextension enable --py widgetsnbextension --sys-prefix && \
    jupyter nbextension enable toc2/main --sys-prefix && \
    jupyter nbextension enable execute_time/ExecuteTime --sys-prefix && \
    jupyter nbextension enable python-markdown/main --sys-prefix && \
    jupyter nbextension enable codefolding/main --sys-prefix && \
    jupyter nbextension enable autosavetime/main --sys-prefix && \
    jupyter nbextension enable tree-filter/index --sys-prefix && \
    jupyter nbextension enable hide_input_all/main --sys-prefix && \
    jupyter nbextension enable hinterland/hinterland --sys-prefix && \
    jupyter nbextension enable varInspector/main --sys-prefix && \
    jupyter nbextension enable spellchecker/main --sys-prefix && \
    jupyter nbextension enable toggle_all_line_numbers/main --sys-prefix && \
    jupyter nbextension enable --py qgrid --sys-prefix && \
    jupyter nbextension install https://github.com/drillan/jupyter-black/archive/master.zip --sys-prefix && \
    jupyter nbextension install --py nbdime --sys-prefix && \
    jupyter nbextension enable jupyter-black-master/jupyter-black --sys-prefix && \
    jupyter nbextension enable nbdime --py --sys-prefix && \
    jupyter nbextension enable rise --py --sys-prefix && \
    jupyter nbextensions_configurator enable --sys-prefix && \
    jupyter labextension install @jupyterlab/git --no-build && \
    jupyter labextension install @jupyterlab/github --no-build && \
    jupyter labextension enable git && \
    jupyter lab build --dev-build=False