#!/usr/bin/env python
"""Add 'from __future__ import annotations' to files that need it."""

import os
import re

FILES = [
    "msticpy/__init__.py",
    "msticpy/analysis/anomalous_sequence/model.py",
    "msticpy/analysis/anomalous_sequence/utils/cmds_only.py",
    "msticpy/analysis/anomalous_sequence/utils/cmds_params_only.py",
    "msticpy/analysis/anomalous_sequence/utils/cmds_params_values.py",
    "msticpy/analysis/anomalous_sequence/utils/data_structures.py",
    "msticpy/analysis/anomalous_sequence/utils/probabilities.py",
    "msticpy/analysis/observationlist.py",
    "msticpy/analysis/timeseries.py",
    "msticpy/auth/cloud_mappings.py",
    "msticpy/auth/secret_settings.py",
    "msticpy/common/data_types.py",
    "msticpy/common/data_utils.py",
    "msticpy/common/pkg_config.py",
    "msticpy/common/timespan.py",
    "msticpy/common/utility/package.py",
    "msticpy/common/wsconfig.py",
    "msticpy/config/ce_common.py",
    "msticpy/config/ce_data_providers.py",
    "msticpy/config/ce_provider_base.py",
    "msticpy/config/comp_edit.py",
    "msticpy/config/compound_ctrls.py",
    "msticpy/config/file_browser.py",
    "msticpy/config/mp_config_control.py",
    "msticpy/config/mp_config_edit.py",
    "msticpy/data/core/query_defns.py",
    "msticpy/data/core/query_provider_utils_mixin.py",
    "msticpy/data/core/query_template.py",
    "msticpy/data/drivers/driver_base.py",
    "msticpy/data/drivers/elastic_driver.py",
    "msticpy/data/drivers/local_data_driver.py",
    "msticpy/data/drivers/local_osquery_driver.py",
    "msticpy/data/drivers/local_velociraptor_driver.py",
    "msticpy/data/drivers/mordor_driver.py",
    "msticpy/data/drivers/prismacloud_driver.py",
    "msticpy/data/drivers/security_graph_driver.py",
    "msticpy/data/drivers/splunk_driver.py",
    "msticpy/data/drivers/sumologic_driver.py",
    "msticpy/data/storage/azure_blob_storage.py",
    "msticpy/data/uploaders/splunk_uploader.py",
    "msticpy/datamodel/entities/account.py",
    "msticpy/datamodel/entities/alert.py",
    "msticpy/datamodel/entities/azure_resource.py",
    "msticpy/datamodel/entities/cloud_application.py",
    "msticpy/datamodel/entities/cloud_logon_session.py",
    "msticpy/datamodel/entities/dns.py",
    "msticpy/datamodel/entities/entity_graph.py",
    "msticpy/datamodel/entities/file.py",
    "msticpy/datamodel/entities/geo_location.py",
    "msticpy/datamodel/entities/graph_property.py",
    "msticpy/datamodel/entities/host.py",
    "msticpy/datamodel/entities/iot_device.py",
    "msticpy/datamodel/entities/mail_cluster.py",
    "msticpy/datamodel/entities/mail_message.py",
    "msticpy/datamodel/entities/mailbox.py",
    "msticpy/datamodel/entities/mailbox_configuration.py",
    "msticpy/datamodel/entities/malware.py",
    "msticpy/datamodel/entities/network_connection.py",
    "msticpy/datamodel/entities/oauth_application.py",
    "msticpy/datamodel/entities/process.py",
    "msticpy/datamodel/entities/registry_key.py",
    "msticpy/datamodel/entities/registry_value.py",
    "msticpy/datamodel/entities/security_group.py",
    "msticpy/datamodel/entities/service_principal.py",
    "msticpy/datamodel/entities/submission_mail.py",
    "msticpy/datamodel/entities/threat_intelligence.py",
    "msticpy/datamodel/entities/url.py",
    "msticpy/datamodel/soc/incident.py",
    "msticpy/init/azure_synapse_tools.py",
    "msticpy/init/logging.py",
    "msticpy/init/mp_pandas_accessors.py",
    "msticpy/init/mp_plugins.py",
    "msticpy/init/pivot.py",
    "msticpy/init/pivot_core/pivot_pipeline.py",
    "msticpy/init/pivot_init/vt_pivot.py",
    "msticpy/nbwidgets/get_text.py",
    "msticpy/nbwidgets/lookback.py",
    "msticpy/nbwidgets/option_buttons.py",
    "msticpy/nbwidgets/select_item.py",
    "msticpy/nbwidgets/select_subset.py",
    "msticpy/transform/auditdextract.py",
    "msticpy/transform/base64unpack.py",
    "msticpy/transform/cmd_line.py",
    "msticpy/transform/network.py",
    "msticpy/transform/proc_tree_build_mde.py",
    "msticpy/transform/process_tree_utils.py",
    "msticpy/vis/data_viewer.py",
    "msticpy/vis/data_viewer_panel.py",
    "msticpy/vis/entity_graph_tools.py",
    "msticpy/vis/matrix_plot.py",
    "msticpy/vis/mordor_browser.py",
    "msticpy/vis/mp_pandas_plot.py",
    "msticpy/vis/nbdisplay.py",
    "msticpy/vis/network_plot.py",
    "msticpy/vis/ti_browser.py",
    "msticpy/vis/timeline.py",
    "msticpy/vis/timeline_common.py",
    "msticpy/vis/timeline_duration.py",
    "msticpy/vis/timeline_values.py",
    "msticpy/vis/vtobject_browser.py",
]

IMPORT_LINE = "from __future__ import annotations"


def add_future_annotations(fpath: str) -> bool:
    """Add the future annotations import to a file."""
    if not os.path.exists(fpath):
        print(f"MISSING: {fpath}")
        return False

    with open(fpath, encoding="utf-8") as f:
        content = f.read()

    if IMPORT_LINE in content:
        print(f"ALREADY HAS: {fpath}")
        return False

    # Find position after docstring following the license header
    # Pattern: license header block, then docstring
    pattern = r'^(# -{50,}.*?# -{50,}\n)(""".*?""")\n'
    match = re.search(pattern, content, re.DOTALL | re.MULTILINE)

    if match:
        insert_pos = match.end()
        new_content = content[:insert_pos] + IMPORT_LINE + "\n\n" + content[insert_pos:]
        with open(fpath, "w", encoding="utf-8") as f:
            f.write(new_content)
        print(f"Updated: {fpath}")
        return True

    print(f"NO MATCH: {fpath}")
    return False


if __name__ == "__main__":
    count = sum(1 for f in FILES if add_future_annotations(f))
    print(f"\nTotal updated: {count}")
