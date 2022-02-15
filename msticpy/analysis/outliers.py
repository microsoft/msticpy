# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Outlier detection class. TODO **Preliminary**.

Similar to the eventcluster module but a little bit more experimental
(read 'less tested'). It uses SkLearn Isolation Forest to identify
outlier events in a single data set or using one data set as training
data and another on which to predict outliers.

"""

import math
from typing import List, Tuple

import numpy as np
import pandas as pd

from .._version import VERSION
from ..common.exceptions import MsticpyImportExtraError

try:
    import matplotlib.pyplot as plt
    from sklearn.ensemble import IsolationForest
except ImportError as imp_err:
    raise MsticpyImportExtraError(
        "Cannot use this feature without Sklearn and matplotlib installed",
        title="Error importing Scikit Learn and matplotlib",
        extra="ml",
    ) from imp_err

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=invalid-name
def identify_outliers(
    x: np.ndarray, x_predict: np.ndarray, contamination: float = 0.05
) -> Tuple[IsolationForest, np.ndarray, np.ndarray]:
    """
    Identify outlier items using SkLearn IsolationForest.

    Arguments
    ---------
    x : np.ndarray
            Input data
    x_predict : np.ndarray
        Model
    contamination : float
        Percentage contamination (default: {0.05})

    Returns
    -------
    Tuple[IsolationForest, np.ndarray, np.ndarray]
        IsolationForest model, X_Outliers,
        y_pred_outliers

    """
    # pylint: disable=no-member
    rng = np.random.RandomState(42)

    # fit the model
    rows, cols = x.shape
    max_samples = min(100, cols)
    max_features = math.floor(math.sqrt(rows))
    clf = IsolationForest(
        max_samples=max_samples,
        max_features=max_features,
        random_state=rng,
        contamination=contamination,
    )

    # fit and train the model
    clf.fit(x)
    clf.predict(x)

    y_pred_outliers = clf.predict(x_predict)

    x_outliers = x_predict[y_pred_outliers == -1]
    return clf, x_outliers, y_pred_outliers


# pylint: disable=too-many-arguments, too-many-locals
def plot_outlier_results(
    clf: IsolationForest,
    x: np.ndarray,
    x_predict: np.ndarray,
    x_outliers: np.ndarray,
    feature_columns: List[int],
    plt_title: str,
):
    """
    Plot Isolation Forest results.

    Parameters
    ----------
    clf : IsolationForest
        Isolation Forest model
    x : np.ndarray
        Input data
    x_predict : np.ndarray
        Prediction
    x_outliers : np.ndarray
        Set of outliers
    feature_columns : List[int]
        list of feature columns to display
    plt_title : str
        Plot title

    """
    # plot the line, the samples, and the nearest vectors to the plane
    x_max_x = x[:, 0].max() + (x[:, 0].max() / 10)
    x_min_x = -x[:, 0].max() / 10
    x_max_y = x[:, 1].max() + (x[:, 1].max() / 10)
    x_min_y = -x[:, 1].max() / 10
    xx, yy = np.meshgrid(
        np.linspace(x_min_x, x_max_x, 100), np.linspace(x_min_y, x_max_y, 100)
    )
    z = clf.decision_function(np.c_[xx.ravel(), yy.ravel()])
    z = z.reshape(xx.shape)

    plt.rcParams["figure.figsize"] = (20, 10)

    plt.title(plt_title)
    # pylint: disable=no-member
    plt.contourf(xx, yy, z, cmap=plt.cm.Blues_r)

    b1 = plt.scatter(x[:, 0], x[:, 1], c="white", s=20, edgecolor="k")
    b2 = plt.scatter(x_predict[:, 0], x_predict[:, 1], c="green", s=40, edgecolor="k")
    c = plt.scatter(
        x_outliers[:, 0], x_outliers[:, 1], c="red", marker="x", s=200, edgecolor="k"
    )
    plt.axis("tight")

    xp_max_x = x_predict[:, 0].max() + (x_predict[:, 0].max() / 10)
    xp_min_x = -x_predict[:, 0].max() / 10
    xp_max_y = x_predict[:, 1].max() + (x_predict[:, 1].max() / 10)
    xp_min_y = -x_predict[:, 1].max() / 10

    plt.xlim((xp_min_x, xp_max_x))
    plt.ylim((xp_min_y, xp_max_y))
    plt.xlabel(feature_columns[0])
    plt.ylabel(feature_columns[1])

    plt.legend(
        [b1, b2, c],
        [
            "training observations",
            "new regular observations",
            "new abnormal observations",
        ],
        loc="upper right",
    )
    plt.show()


def remove_common_items(data: pd.DataFrame, columns: List[str]) -> pd.DataFrame:
    """
    Remove rows from input DataFrame.

    Parameters
    ----------
    data : pd.DataFrame
        Input dataframe
    columns : List[str]
        Column list to filter

    Returns
    -------
    pd.DataFrame
        Filtered DataFrame

    """
    filtered_df = data
    # pylint: disable=cell-var-from-loop
    for col in columns:
        filtered_df = filtered_df.filter(
            lambda x: (x[col].std() == 0 and x[col].count() > 10)
        )

    return filtered_df
