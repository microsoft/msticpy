# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pivot browser widget."""
from typing import Dict, List

import ipywidgets as widgets
from IPython import get_ipython
from IPython.display import display

from ..._version import VERSION
from ...datamodel import entities

try:
    import pyperclip

    _ENABLE_CLIP = True
except ImportError:
    _ENABLE_CLIP = False

__version__ = VERSION
__author__ = "Ian Hellen"


def _get_entities_with_pivots():
    piv_entities = {}
    for entity_name in dir(entities):
        entity = getattr(entities, entity_name)
        if not isinstance(entity, type) or not issubclass(entity, entities.Entity):
            continue
        piv_list = sorted(entity.get_pivot_list())
        if piv_list:
            piv_entities[entity_name] = entity, piv_list
    return piv_entities


def _box_layout(width="95%"):
    return widgets.Layout(
        **{
            "width": width,
            "border": "solid gray 1px",
            "margin": "1pt",
            "padding": "5pt",
        }
    )


_GENERIC_HELP = """
<p><b>Call with string/simple value:</b></p>
<p><i>Entity.categ.func</i>(<i>arg</i>)</p>
<p><code>IpAddress.util.whois("192.168.1.1")</code></p>

<p><b>Call with list:</b>
<p><i>Entity.categ.func</i>(<i>list</i>)</p>
<p><code>IpAddress.util.ip_type(ip_list1)</code></p>

<p><b>Call with entity:</b></p>
<p><i>Entity.categ.func</i>(<i>entity</i>)</p>
<p><code>IpAddress.util.ip_type(ip_entity)</code></p>

<p><b>Call with dataframe:</b></p>
<p><i>Entity.categ.func</i>(data=<i>df</i>, column=<i>col_name</i>)</p>
<p><code>IpAddress.util.ip_type(data=ip_df, column="ip")</code></p>
<hr>
<p><b>Note:</b> when calling data queries, instead of using generic parameter
names like 'column' or value, you must use the parameter name
expected by the function. E.g. '..func(host_name="myhost")',
func(data=my_df, account_name="ian")', etc.</p>
<p>See the function-specific help for more details</p>

"""


class PivotBrowser:
    """Pivot entity browser class."""

    def __init__(self):
        """Create an instance of the Pivot browser."""
        self._text: Dict[str, widgets.Widget] = {}
        self._select: Dict[str, widgets.Widget] = {}
        self._layout: Dict[str, widgets.Widget] = {}
        self._html: Dict[str, widgets.Widget] = {}
        self._btn: Dict[str, widgets.Widget] = {}

        self.piv_entities: Dict[str, List[str]] = _get_entities_with_pivots()

        self._create_select_controls()
        self._create_help_controls()
        self._create_search_controls()

        deflt_funcs = next(iter(self.piv_entities.values()))[1][0]
        self._select_function({"new": deflt_funcs})
        self._search_func(change={"new": ""})
        self.layout = widgets.VBox(
            [
                self._layout["entity_funcs"],
                self._layout["search_box"],
                self._layout["disp_btn_hbox"],
                self._layout["func_help_accd"],
            ]
        )

    def _create_select_controls(self):
        """Instantiate and set up select widgets."""
        self._select["pivot_funcs"] = widgets.Select(
            description="pivot function",
            options=next(iter(self.piv_entities.values()))[1],
            layout=widgets.Layout(height="300px", width="95%"),
        )

        self._select["pivot_funcs"].observe(self._select_function, names="value")

        self._select["entities"] = widgets.Select(
            description="entity",
            options=list(self.piv_entities.keys()),
            layout=widgets.Layout(height="300px", width="70%"),
        )

    def _create_help_controls(self):
        """Instantiate and set up help widgets."""
        self._html["gen_help"] = widgets.HTML(_GENERIC_HELP)
        self._html["func_help"] = widgets.HTML("")
        self._html["search_res"] = widgets.HTML("")
        self._layout["func_help_accd"] = widgets.Accordion(
            children=[
                self._html["gen_help"],
                self._html["func_help"],
                self._html["search_res"],
            ],
            layout=_box_layout("95%"),
        )
        self._layout["func_help_accd"].set_title(0, "Generic Pivot function help")
        self._layout["func_help_accd"].set_title(1, "Function-specific help")
        self._layout["func_help_accd"].set_title(2, "Search results (0)")
        self._layout["func_help_accd"].selected_index = None

        self._select["entities"].observe(self._display_pivots, names="value")

        self._layout["entity_box"] = widgets.VBox(
            [widgets.HTML("<b>Entities</b>"), self._select["entities"]],
            layout=_box_layout("40%"),
        )
        self._layout["funcs_box"] = widgets.VBox(
            [
                widgets.HTML("<b>Selected entity pivot Functions</b>"),
                self._select["pivot_funcs"],
            ],
            layout=_box_layout("55%"),
        )
        self._layout["entity_funcs"] = widgets.HBox(
            [self._layout["entity_box"], self._layout["funcs_box"]],
            layout=widgets.Layout(width="100%"),
        )

    def _create_search_controls(self):
        """Instantiate and set up search widgets."""
        self._text["search_txt"] = widgets.Text(
            description="Search:",
            style={"description_width": "initial"},
            layout=widgets.Layout(width="95%"),
            continuous_update=False,
        )
        self._layout["search_box"] = widgets.HBox(
            [self._text["search_txt"]], layout=_box_layout("95%")
        )
        self._text["search_txt"].observe(self._search_func, names="value")
        self._html["cur_func_title"] = widgets.HTML(
            description="Current function:", style={"description_width": "initial"}
        )
        self._btn["copy"] = widgets.Button(description="Copy to clipboard")
        self._btn["copy"].on_click(self._copy_to_clip)
        self._btn["copy"].enabled = _ENABLE_CLIP
        self._layout["disp_btn_hbox"] = widgets.HBox(
            [self._html["cur_func_title"], self._btn["copy"]], layout=_box_layout("95%")
        )

    def display(self):
        """Display the widget."""
        display(self.layout)

    def _ipython_display_(self):
        """Display in IPython."""
        self.display()

    def _display_pivots(self, change):
        entity = change.get("new")
        self._select["pivot_funcs"].options = self.piv_entities.get(entity)[1]

    def _select_function(self, change):
        func_name = change.get("new")
        entity_name = self._select["entities"].value
        entity = self.piv_entities[entity_name][0]
        func_name_path = func_name.split(".")
        obj = entity
        for path in func_name_path:
            obj = getattr(obj, path)
        doc_str = [
            "<i>Note: use generic help and parameters listed "
            + "there for most common use of pivot functions</i>",
            "",
            f"<b>{entity_name}.{func_name}</b>",
            obj.__doc__.replace("\n", "<br>").replace(" ", "&nbsp;"),
        ]

        self._html["func_help"].value = "<br>".join(doc_str)
        self._html["cur_func_title"].value = f"<code>{self._get_current_func()}</code>"

    def _get_current_func(self):
        func_name = self._select["pivot_funcs"].value
        entity_name = self._select["entities"].value
        return f"{entity_name}.{func_name}()"

    def _add_cur_func_to_cell(self, btn):
        del btn
        get_ipython().set_next_input(self._get_current_func())

    def _copy_to_clip(self, btn):
        del btn
        if _ENABLE_CLIP:
            pyperclip.copy(self._get_current_func())

    def _search_func(self, change):
        search_txt = change.get("new")
        search_results, hits = self._search_funcs(search_txt)
        self._layout["func_help_accd"].set_title(2, f"Search results ({hits})")
        self._html["search_res"].value = search_results

    def _search_funcs(self, search_txt):
        f_entities = {
            ent for ent in self.piv_entities if search_txt.casefold() in ent.casefold()
        }
        matching_funcs = [
            (ent, func)
            for ent, ent_piv in self.piv_entities.items()
            for func in ent_piv[1]
            if search_txt.casefold() in func.casefold()
        ]

        hit_count = len(f_entities)
        ent_with_funcs = {ent[0] for ent in matching_funcs}
        f_entities.update(ent_with_funcs)

        ent_func_list = []
        for entity in f_entities:
            ent_func_list.append(f"<b>{entity}</b>")
            if entity in ent_with_funcs:
                ent_funcs = [item[1] for item in matching_funcs if item[0] == entity]
                ent_func_list.extend(ent_funcs)
                hit_count += len(ent_funcs)
        return "<br>".join(ent_func_list), hit_count
