# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Miscellaneous helper methods for Jupyter Notebooks."""
from typing import List, Tuple, Union

from IPython.display import display
from IPython import get_ipython

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


# placeholder for pkg_config.get_config - this function is
# overwritten by msticp.common.pkg_config
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

        for arg in args:
            self._output.append(arg)

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

    @property
    def help_uri(self) -> Union[Tuple[str, str], str]:
        """Get the default help URI."""
        return self.DEF_HELP_URI

    def _display_exception(self):
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
        content = ""
        for line in self._output:
            if isinstance(line, tuple):
                l_content, l_type = line
                if l_type == "title":
                    content += f"<h3><p class='title'>{l_content}</p></h3>"
                if l_type == "uri":
                    if isinstance(l_content, tuple):
                        name, uri = l_content
                    else:
                        name = uri = l_content
                    content += (
                        f"<ul class='circle'><li><a href='{uri}'>{name}</a></li></ul>"
                    )
            else:
                text_line = line.replace("\n", "<br>")
                content += f"{text_line}<br>"

        ex_html = "".join((ex_style, div_tmplt.format(content=content)))
        return ex_html

    def _display_txt_exception(self):
        """Display text-only version of the exception text."""
        for line in self._output:
            if isinstance(line, tuple):
                l_content, l_type = line
                if l_type == "title":
                    print("-" * len(l_content))
                    print(l_content)
                    print("-" * len(l_content))
                if l_type == "uri":
                    if isinstance(l_content, tuple):
                        print(f" - {': '.join(l_content)}")
                    else:
                        print(f" - {l_content}")
            else:
                print(line)


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
        if args:
            add_args = [*args, *mp_loc_mssg]
        else:
            add_args = [def_mssg, *mp_loc_mssg]
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
            "Please verfiy that a valid KeyVault section has been configured"
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
            "Please verfiy that the item using this secret is properly"
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


class MsticpyConnectionError(MsticpyUserError):
    """Exception class for KqlConnection errors."""

    DEF_HELP_URI = (
        "DataProviders",
        "https://msticpy.readthedocs.io/en/latest/DataProviders.html",
    )


class MsticpyKqlConnectionError(MsticpyUserError):
    """Exception class for KqlConnection errors."""

    DEF_HELP_URI = (
        "Connecting to Azure Sentinel",
        "https://msticpy.readthedocs.io/en/latest/DataProviders.html"
        + "#connecting-to-an-azure-sentinel-workspace",
    )


def is_ipython() -> bool:
    """
    Return True if running in IPython environment.

    Returns
    -------
    bool
        True if running in IPython environment,
        otherwise False

    """
    return bool(get_ipython())
