# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Outlier detection class. TODO **Preliminary**."""

import math

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.ensemble import IsolationForest

from .. _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'

# pylint: disable=all
# noqa


def identify_outliers(X: np.array, X_predict: np.Array = None, contamination: float = 0.05):
    """
    Identify outlier items using SkLearn IsolationForest.

    Arguments:
        X {[np.array]} -- Input data

    Keyword Arguments:
        X_predict {[np.array]} -- Model (default: {None})
        contamination {float} -- Percentage contamination (default: {0.05})
    """
    rng = np.random.RandomState(42)

    # fit the model
    rows, cols = X.shape
    max_samples = min(100, cols)
    max_features = math.floor(math.sqrt(rows))
    clf = IsolationForest(max_samples=max_samples, max_features=max_features,
                          random_state=rng, contamination=contamination)
    clf.fit(X)
    y_pred_train = clf.predict(X)

    y_pred_outliers = clf.predict(X_predict)

    X_outliers = X_predict[y_pred_outliers == -1]
    return clf, X_outliers, y_pred_outliers


def plot_outlier_results(clf: IsolationForest, X: np.array,
                         X_predict: np.array, X_outliers: np.array,
                         feature_columns: list({int}), plt_title: str):
    """
    Plot Isolation Forest results.

    Arguments:
        clf {IsolationForest} -- Isolation Forest model
        X {np.array} -- Input data
        X_predict {np.array} -- Prediction
        X_outliers {np.array} -- Set of outliers
        feature_columns {list} -- list of feature columns to display
        plt_title {str} -- Title
    """
    # plot the line, the samples, and the nearest vectors to the plane
    x_max_x = X[:, 0].max() + (X[:, 0].max() / 10)
    x_min_x = - X[:, 0].max() / 10
    x_max_y = X[:, 1].max() + (X[:, 1].max() / 10)
    x_min_y = - X[:, 1].max() / 10
    xx, yy = np.meshgrid(np.linspace(x_min_x, x_max_x, 100),
                         np.linspace(x_min_y, x_max_y, 100))
    Z = clf.decision_function(np.c_[xx.ravel(), yy.ravel()])
    Z = Z.reshape(xx.shape)

    plt.rcParams['figure.figsize'] = (20, 10)

    plt.title(plt_title)
    plt.contourf(xx, yy, Z, cmap=plt.cm.Blues_r)

    b1 = plt.scatter(X[:, 0], X[:, 1], c='white',
                     s=20, edgecolor='k')
    b2 = plt.scatter(X_predict[:, 0], X_predict[:, 1], c='green',
                     s=40, edgecolor='k')
    c = plt.scatter(X_outliers[:, 0], X_outliers[:, 1], c='red', marker='x',
                    s=200, edgecolor='k')
    plt.axis('tight')

    xp_max_x = X_predict[:, 0].max() + (X_predict[:, 0].max() / 10)
    xp_min_x = - X_predict[:, 0].max() / 10
    xp_max_y = X_predict[:, 1].max() + (X_predict[:, 1].max() / 10)
    xp_min_y = - X_predict[:, 1].max() / 10

    plt.xlim((xp_min_x, xp_max_x))
    plt.ylim((xp_min_y, xp_max_y))
    plt.xlabel(feature_columns[0])
    plt.ylabel(feature_columns[1])

    plt.legend([b1, b2, c],
               ["training observations",
                "new regular observations", "new abnormal observations"],
               loc="upper right")
    plt.show()


def remove_common_items(data: pd.DataFrame, columns: list):
    """
    Remove rows from input DataFrame.

    Arguments:
        data {pd.DataFrame} -- [description]
        columns {list} -- [description]
    """
    filtered_df = data
    for col in columns:
        filtered_df = filtered_df.filter(lambda x: (x[col].std() == 0 and
                                                    x[col].count() > 10))

    return filtered_df
