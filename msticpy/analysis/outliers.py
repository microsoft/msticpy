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
from typing import List, Optional, Tuple, Union

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
__author__ = "Ian Hellen, Tatsuya Hasegawa"


# pylint: disable=invalid-name
def identify_outliers(
    x: np.ndarray,
    x_predict: np.ndarray,
    contamination: float = 0.05,
    max_features: Optional[Union[int, float]] = None,
) -> Tuple[IsolationForest, np.ndarray, np.ndarray]:
    """
    Identify outlier items using SkLearn IsolationForest.

    Arguments:
    ---------
    x : np.ndarray
            Input data
    x_predict : np.ndarray
        Model
    contamination : float
        Percentage contamination (default: {0.05})
    max_features : int or float, optional
        Specifies max num or max rate of features
        to be randomly selected when building each tree.
        default: None => {math.floor(math.sqrt(cols))}

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
    max_samples = min(100, rows)
    if not max_features:
        max_features = math.floor(math.sqrt(cols))

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
    if len(feature_columns) == 2:
        # two dimension plot: mostly remain original codes from msticpy 2.14.0
        # plot the line, the samples, and the nearest vectors to the plane
        x_max_x = x[:, 0].max() + (x[:, 0].max() / 10)
        x_min_x = -x[:, 0].max() / 10
        x_max_y = x[:, 1].max() + (x[:, 1].max() / 10)
        x_min_y = -x[:, 1].max() / 10
        xx, yy = np.meshgrid(
            np.linspace(x_min_x, x_max_x, 100), np.linspace(x_min_y, x_max_y, 100)
        )
        z = clf.decision_function(
            np.c_[
                xx.ravel(),
                yy.ravel(),
                np.zeros(
                    (xx.ravel().shape[0], clf.n_features_in_ - len(feature_columns))
                ),
            ]
        )
        z = z.reshape(xx.shape)

        plt.rcParams["figure.figsize"] = (20, 10)

        plt.title(plt_title)
        # pylint: disable=no-member
        plt.contourf(xx, yy, z, cmap=plt.cm.Blues_r)  # type: ignore

        b1 = plt.scatter(x[:, 0], x[:, 1], c="white", s=20, edgecolor="k")
        b2 = plt.scatter(
            x_predict[:, 0], x_predict[:, 1], c="green", s=40, edgecolor="k"
        )
        c = plt.scatter(x_outliers[:, 0], x_outliers[:, 1], c="red", marker="x", s=200)
        plt.axis("tight")

        xp_max_x = x_predict[:, 0].max() + (x_predict[:, 0].max() / 10)
        xp_min_x = -x_predict[:, 0].max() / 10
        xp_max_y = x_predict[:, 1].max() + (x_predict[:, 1].max() / 10)
        xp_min_y = -x_predict[:, 1].max() / 10

        plt.xlim((xp_min_x, xp_max_x))
        plt.ylim((xp_min_y, xp_max_y))
        plt.xlabel(feature_columns[0])  # type: ignore
        plt.ylabel(feature_columns[1])  # type: ignore

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

    elif len(feature_columns) > 2:  # multi dimension subplots
        dimension_num = x.shape[1]
        fig, axes = plt.subplots(
            dimension_num, dimension_num, figsize=(20, 20), constrained_layout=True
        )
        for i in range(dimension_num):
            for j in range(dimension_num):
                if i != j:
                    # plot the line, the samples, and the nearest vectors to the plane
                    x_max_x = x[:, j].max() + (x[:, j].max() / 10)
                    x_min_x = -x[:, j].max() / 10
                    x_max_y = x[:, i].max() + (x[:, i].max() / 10)
                    x_min_y = -x[:, i].max() / 10
                    xx, yy = np.meshgrid(
                        np.linspace(x_min_x, x_max_x, 100),
                        np.linspace(x_min_y, x_max_y, 100),
                    )
                    z = clf.decision_function(
                        np.c_[
                            xx.ravel(),
                            yy.ravel(),
                            np.zeros((xx.ravel().shape[0], len(feature_columns) - 2)),
                        ]
                    )
                    z = z.reshape(xx.shape)

                    # pylint: disable=no-member
                    axes[i, j].contourf(xx, yy, z, cmap=plt.cm.Blues_r)  # type: ignore

                    b1 = axes[i, j].scatter(x[:, j], x[:, i], c="white", edgecolor="k")
                    b2 = axes[i, j].scatter(
                        x_predict[:, j], x_predict[:, i], c="green", edgecolor="k"
                    )
                    c = axes[i, j].scatter(
                        x_outliers[:, j], x_outliers[:, i], c="red", marker="x"
                    )

                    xp_max_x = x_predict[:, 0].max() + (x_predict[:, 0].max() / 10)
                    xp_min_x = -x_predict[:, 0].max() / 10
                    xp_max_y = x_predict[:, 1].max() + (x_predict[:, 1].max() / 10)
                    xp_min_y = -x_predict[:, 1].max() / 10

                    axes[i, j].axis(xmin=xp_min_x, xmax=xp_max_x)
                    axes[i, j].axis(ymin=xp_min_y, ymax=xp_max_y)
                    axes[i, j].set_xlabel(f"{feature_columns[j]}")
                    axes[i, j].set_ylabel(f"{feature_columns[i]}")

                else:
                    # do not show the same features x,y each other.
                    axes[i, j].axis("off")

        fig.suptitle(plt_title)
        plt.legend(
            [b1, b2, c],
            [
                "training observations",
                "new regular observations",
                "new abnormal observations",
            ],
            facecolor="#0072BD",
            framealpha=0.3,
        )
        plt.show()

    else:
        raise ValueError("plot_outlier_results function needs more than two features.")


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
            lambda x: (x[col].std() == 0 and x[col].count() > 10)  # type: ignore
        )

    return filtered_df
