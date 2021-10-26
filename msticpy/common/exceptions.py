# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Miscellaneous helper methods for Jupyter Notebooks."""
import contextlib
from typing import List, Tuple, Union

from IPython.display import display

from .utility import is_ipython
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


# placeholder for pkg_config.get_config - this function is
# overwritten by msticpy.common.pkg_config
def _get_config(setting_path: str):
    del setting_path
    return True


# Standard exception types
class MsticpyException(Exception):
    """Default exception class for msticpy."""


class MsticpyConfigException(MsticpyException):
    """Configuration exception class for msticpy."""


class MsticpyResourceException(MsticpyException):
    """Exception class for resource errors."""


######################################
# User-friendly displayable exceptions
# ------------------------------------
# Note: for ease of distinguishing the two exception types
# name any classes derived from MsticpyUserError with an "Error"
# suffix. Name classes derived from MsticpyException with an
# "Exception" suffix
class MsticpyUserError(MsticpyException):
    """Msticpy User exception displaying friendly message."""

    _display_exceptions = True

    DEF_HELP_URI = ("msticpy documentation", "https://msticpy.readthedocs.org")

    def __init__(
        self, *args, help_uri: Union[Tuple[str, str], str, None] = None, **kwargs
    ):
        """
        Create an instance of the MsticpyUserError class.

        Parameters
        ----------
        args : Iterable of strings
            Args will be printed as text of the exception.
        help_uri : Union[Tuple[str, str], str, None], optional
            Primary URL, by default "https://msticpy.readthedocs.org"

        Other Parameters
        ----------------
        title : str
            If a `title` keyword argument is supplied it will be used
            to create the title line.
        *_uri : str
            Additional keyword arguments who's names end in "_uri"
            will be used to create a list of references in addition to
            the primary `help_uri`

        Notes
        -----
        The exception text is displayed when the exception is created
        and *not* when it is raised. We recommend creating the exception
        within the `raise` statement. E.g.

        `raise MsticpyUserException(arg1, arg2...)`

        Developer note:
        Any classes derived from MsticpyUserError should be named with
        an "Error" suffix to distinguish these from standard exception types.

        """
        # This nasty-looking thing just means that this is a list that
        # holds:
        # just strings - for simple args strings
        # tuples(str, str) - if the item is annotated as a uri or title
        # tuple(tuple(str, str), str) - if the URI is a tuple of display_name, URI
        self._output: List[
            Union[str, Tuple[str, str], Tuple[Tuple[str, str], str]]
        ] = []
        title = kwargs.pop("title", "we've hit an error while running")
        self._output.append((f"{self.__class__.__name__} - {title}", "title"))

        self._output.extend(args)

        self._output.append("\nFor more help on fixing this error see:")
        if not help_uri:
            help_uri = self.DEF_HELP_URI
        self._output.append((help_uri, "uri"))  # type: ignore

        help_args = [
            kw_val for kw_arg, kw_val in kwargs.items() if kw_arg.endswith("_uri")
        ]
        if help_args:
            self._output.append("You can find other related help here:")
            for uri in help_args:
                self._output.append((uri, "uri"))
        if _get_config("msticpy.FriendlyExceptions"):
            self._display_exception()

        # add the extra elements to the the exception standard args.
        ex_args = [title, *args, help_uri, *help_args]
        super().__init__(*ex_args)

    @classmethod
    @contextlib.contextmanager
    def no_display_exceptions(cls):
        """Context manager to block exception display to IPython/stdout."""
        cls._display_exceptions = False
        yield
        cls._display_exceptions = True

    @property
    def help_uri(self) -> Union[Tuple[str, str], str]:
        """Get the default help URI."""
        return self.DEF_HELP_URI

    def _display_exception(self):
        if not self._display_exceptions:
            return
        if is_ipython():
            display(self)
        else:
            self._display_txt_exception()

    def _repr_html_(self):
        """Return HTML-formatted exception text."""
        ex_style = """
        <style>
            div.solid {border: thin solid black; padding:10px;}
            p.title {background-color:Tomato; padding:5px;}
            ul.circle {list-style-type: circle;}
        </style>
        """
        div_tmplt = "<div class='solid'>{content}</div>"
        about_blank = "target='_blank' rel='noopener noreferrer'"
        content = []
        for line in self._output:
            if isinstance(line, tuple):
                l_content, l_type = line
                if l_type == "title":
                    content.append(f"<h3><p class='title'>{l_content}</p></h3>")
                elif l_type == "uri":
                    if isinstance(l_content, tuple):
                        name, uri = l_content
                    else:
                        name = uri = l_content
                    content.append(
                        f"<ul class='circle'><li><a href='{uri}' {about_blank}>"
                        f"{name}</a></li></ul>"
                    )
            else:
                text_line = line.replace("\n", "<br>")
                content.append(f"{text_line}<br>")

        return "".join((ex_style, div_tmplt.format(content="".join(content))))

    def _display_txt_exception(self):
        """Display text-only version of the exception text."""
        print(self._get_exception_text())

    def _get_exception_text(self) -> str:
        out_lines = []
        for line in self._output:
            if isinstance(line, tuple):
                l_content, l_type = line
                if isinstance(l_content, tuple):
                    l_content = l_content[0]
                if l_type == "title":
                    out_lines.append("-" * len(l_content))
                    out_lines.append(l_content)
                    out_lines.append("-" * len(l_content))
                elif l_type == "uri":
                    if isinstance(l_content, tuple):
                        out_lines.append(f" - {': '.join(l_content)}")
                    else:
                        out_lines.append(f" - {l_content}")
            else:
                out_lines.append(line)
        return "\n".join(out_lines)


class MsticpyUserConfigError(MsticpyUserError):
    """Configuration user exception class for msticpy."""

    DEF_HELP_URI = (
        "Configuring msticpy",
        "https://msticpy.readthedocs.io/en/latest/getting_started/msticpyconfig.html",
    )

    def __init__(
        self, *args, help_uri: Union[Tuple[str, str], str, None] = None, **kwargs
    ):
        """
        Create generic user configuration exception.

        Parameters
        ----------
        help_uri : Union[Tuple[str, str], str, None], optional
            Override the default help URI.

        """
        def_mssg = "There is a problem with configuration in your msticpyconfig.yaml."
        mp_loc_mssg = [
            "Ensure that the path to your msticpyconfig.yaml is specified with"
            + " the MSTICPYCONFIG environment variable.",
            "Or ensure that a copy of this file is in the current directory.",
        ]
        add_args = [*args, *mp_loc_mssg] if args else [def_mssg, *mp_loc_mssg]
        if help_uri:
            uri: Union[Tuple[str, str], str] = help_uri
            add_uris = {"basehelp_uri": self.DEF_HELP_URI}
        else:
            uri = self.DEF_HELP_URI
            add_uris = {}
        super().__init__(*add_args, help_uri=uri, **add_uris, **kwargs)


class MsticpyKeyVaultConfigError(MsticpyUserConfigError):
    """Key Vault configuration exception."""

    DEF_HELP_URI = (
        "Using keyvault to store msticpy secrets",
        "https://msticpy.readthedocs.io/en/latest/getting_started/msticpyconfig.html"
        + "#specifying-secrets-as-key-vault-secrets",
    )

    def __init__(
        self, *args, help_uri: Union[Tuple[str, str], str, None] = None, **kwargs
    ):
        """
        Create Key Vault configuration exception.

        Parameters
        ----------
        help_uri : Union[Tuple[str, str], str, None], optional
            Override the default help URI.

        """
        mssg = (
            "Please verify that a valid KeyVault section has been configured"
            + "in your msticpyconfig.yaml."
        )
        add_args = [*args, mssg]
        uri = help_uri or self.DEF_HELP_URI
        super().__init__(*add_args, help_uri=uri, **kwargs)


class MsticpyKeyVaultMissingSecretError(MsticpyKeyVaultConfigError):
    """Missing secret exception."""

    def __init__(
        self, *args, help_uri: Union[Tuple[str, str], str, None] = None, **kwargs
    ):
        """
        Create Key Vault missing key exception.

        Parameters
        ----------
        help_uri : Union[Tuple[str, str], str, None], optional
            Override the default help URI.

        """
        mssg = (
            "Please verify that the item using this secret is properly"
            + " configured in in your msticpyconfig.yaml."
        )
        add_args = [*args, mssg]
        uri = help_uri or self.DEF_HELP_URI
        super().__init__(*add_args, help_uri=uri, **kwargs)


class MsticpyAzureConfigError(MsticpyUserConfigError):
    """Exception class for AzureData."""

    DEF_HELP_URI = (
        "Using the Azure API connector",
        "https://msticpy.readthedocs.io/en/latest/data_acquisition/AzureData.html"
        + "#instantiating-and-connecting-with-an-azure-data-connector",
    )

    def __init__(
        self, *args, help_uri: Union[Tuple[str, str], str, None] = None, **kwargs
    ):
        """
        Create Azure data missing configuration exception.

        Parameters
        ----------
        help_uri : Union[Tuple[str, str], str, None], optional
            Override the default help URI.

        """
        uri = help_uri or self.DEF_HELP_URI
        super().__init__(*args, help_uri=uri, **kwargs)


class MsticpyNotConnectedError(MsticpyUserError):
    """Exception class for NotConnected errors."""

    DEF_HELP_URI = (
        "Querying and importing data",
        "https://msticpy.readthedocs.io/en/latest/DataAcquisition.html"
        + "#querying-and-importing-data",
    )


class MsticpyNoDataSourceError(MsticpyUserError):
    """Exception class for missing data source errors."""

    DEF_HELP_URI = (
        "Querying and importing data",
        "https://msticpy.readthedocs.io/en/latest/DataAcquisition.html"
        + "#querying-and-importing-data",
    )


class MsticpyDataQueryError(MsticpyUserError):
    """Exception class for data query errors."""

    DEF_HELP_URI = (
        "Query failed",
        "https://msticpy.readthedocs.io/en/latest/DataAcquisition.html"
        + "#querying-and-importing-data",
    )


class MsticpyConnectionError(MsticpyUserError):
    """Exception class for KqlConnection errors."""

    DEF_HELP_URI = (
        "DataProviders",
        "https://msticpy.readthedocs.io/en/latest/data_acquisition/DataProviders.html",
    )


class MsticpyKqlConnectionError(MsticpyUserError):
    """Exception class for KqlConnection errors."""

    DEF_HELP_URI = (
        "Connecting to Azure Sentinel",
        "https://msticpy.readthedocs.io/en/latest/data_acquisition/DataProviders.html"
        + "#connecting-to-an-azure-sentinel-workspace",
    )


class MsticpyImportExtraError(MsticpyUserError, ImportError):
    """Exception class for Imports that need an extra."""

    DEF_HELP_URI = (
        "Installing msticpy",
        "https://msticpy.readthedocs.io/en/latest/getting_started/Installing.html",
    )

    def __init__(
        self, *args, help_uri: Union[Tuple[str, str], str, None] = None, **kwargs
    ):
        """
        Create import missing extra exception.

        Parameters
        ----------
        help_uri : Union[Tuple[str, str], str, None], optional
            Override the default help URI.
        extra : str
            The name of the setup extra that needs to be installed.

        """
        extra = kwargs.pop("extra", None)
        if not extra:
            raise AttributeError("Keyword argument 'extra' must be supplied")
        mssg = "".join(
            [
                "This feature requires one or more additional packages",
                " to be installed.\n",
                "To do this run the command:\n",
                f"pip install msticpy[{extra}]",
            ]
        )
        add_args = [*args, mssg]
        uri = help_uri or self.DEF_HELP_URI
        super().__init__(*add_args, help_uri=uri, **kwargs)


class MsticpyAzureConnectionError(MsticpyUserError):
    """Exception class for Azure Connection errors."""

    DEF_HELP_URI = (
        "Connecting to Azure Sentinel",
        "https://msticpy.readthedocs.io/en/latest/data_acquisition/AzureData.html"
        + "#instantiating-and-connecting-with-an-azure-data-connector",
    )


class MsticpyParameterError(MsticpyUserError):
    """Exception class for missing/incorrect parameters."""

    DEF_HELP_URI = ("MSTICPy documentation", "https://msticpy.readthedocs.io")

    def __init__(
        self, *args, help_uri: Union[Tuple[str, str], str, None] = None, **kwargs
    ):
        """
        Create parameter exception.

        Parameters
        ----------
        help_uri : Union[Tuple[str, str], str, None], optional
            Override the default help URI.
        parameters : Union[str, List[str]
            The name of the bad parameter(s).

        """
        parameter = kwargs.pop("parameter", None)
        if not parameter:
            raise AttributeError("Keyword argument 'parameter' must be supplied")
        mssg = "One or more parameters were incorrect."
        if isinstance(parameter, str):
            parameter = [parameter]
        add_args = [*args, mssg, ", ".join(parameter)]
        uri = help_uri or self.DEF_HELP_URI
        super().__init__(*add_args, help_uri=uri, **kwargs)
