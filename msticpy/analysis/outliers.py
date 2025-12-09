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

In addition, kLabUM's Robust Random Cut Forest implemented since version 2.16.x.
It's slower than SkLearn Isolation Forest however more considerable for
time series structure and can also process by streaming if you need.

Imported Robust Random Cut Forest python library
- MIT License Copyright (c) 2018 kLabUM
- - https://klabum.github.io/rrcf/

"""

from __future__ import annotations

import logging
import math

import numpy as np
import pandas as pd
from tqdm.auto import tqdm

from .._version import VERSION
from ..common.exceptions import MsticpyImportExtraError

try:
    import matplotlib.pyplot as plt
    from joblib import Parallel, delayed
    from sklearn.ensemble import IsolationForest
except ImportError as imp_err:
    raise MsticpyImportExtraError(
        "Cannot use this feature without Sklearn and matplotlib installed",
        title="Error importing Scikit Learn and matplotlib",
        extra="ml",
    ) from imp_err

try:
    import rrcf
except ImportError as imp_err:
    raise MsticpyImportExtraError(
        "To use RRCF algorism feature for anomaly detection,\
             'pip install rrcf==0.4.4' or pip install msticpy[ml]",
        title="Error importing rrcf (Robust Random Cut Forest)",
        extra="ml",
    ) from imp_err

__version__ = VERSION
__author__ = "Ian Hellen, Tatsuya Hasegawa"

logger: logging.Logger = logging.getLogger(__name__)


# pylint: disable=invalid-name
def identify_outliers(
    x: np.ndarray,
    x_predict: np.ndarray,
    contamination: float = 0.05,
    max_features: int | float | None = None,
    max_samples: int | float | None = None,
) -> tuple[IsolationForest, np.ndarray, np.ndarray]:
    """
    Identify outlier items using SkLearn IsolationForest.

    Parameters
    ----------
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
    max_samples: int or float, optional
        default: None => { min(100,rows) }
        if you want to use more than 100 samples,
        please specify via this option.

    Returns
    -------
    tuple[IsolationForest, np.ndarray, np.ndarray]
        IsolationForest model, x_Outliers, y_pred_outliers

    """
    # pylint: disable=no-member
    rng = np.random.RandomState(42)

    # fit the model
    rows, cols = x.shape

    # Determine sample size
    if max_samples is None:
        n_samples = int(min(100, rows))
    elif isinstance(max_samples, float) and 0 < max_samples < 1:
        n_samples = int(max_samples * rows)
    else:
        n_samples = int(min(max_samples, rows))

    if not max_features:
        max_features = math.floor(math.sqrt(cols))

    logger.info("max_samples parameter => %d", n_samples)
    logger.info("max_features parameter => %d", max_features)

    clf = IsolationForest(
        max_samples=n_samples,
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


class RobustRandomCutForest:
    """
    RobustRandomCutForest Class used in identify_outliers_rrcf().

    With similar structure to SKlearn IsolationForest.
    """

    def __init__(
        self,
        num_trees: int = 100,
        tree_size: int = 256,
        contamination: float = 0.05,
        max_samples: int | float | None = None,
        max_features: int | float | None = None,
    ):
        """
        Initialize Robust Random Cut Forest model.

        Parameters
        ----------
        num_trees: int
            Number of trees in the forest
        tree_size: int
            Maximum number of points in each tree
        contamination: float
            Proportion of outliers in the data
        max_samples: int or float, optional
            Number of samples to build each tree
        max_features: int or float, optional
            Number of features to consider for splitting

        Returns
        -------
        Class instance
            self
        """
        self.num_trees = num_trees
        self.tree_size = tree_size
        self.contamination = contamination
        self.max_samples = max_samples
        self.max_features = max_features
        self._feature_indices = np.ndarray
        self.n_features_in_ = 2
        self.forest: list | None = None
        self._train_scores: np.ndarray
        self._train_samples: np.ndarray

    def _select_features(self, cols: int) -> np.ndarray:
        """
        Randomly select features for each tree.

        Parameters
        ----------
        cols : int
            feature columns

        Returns
        -------
        np.ndarray
            seleted features indices

        """
        if self.max_features is None:
            self.max_features = math.floor(math.sqrt(cols))

        # pylint: disable=no-member
        rng = np.random.RandomState(42)
        return rng.choice(cols, size=int(self.max_features), replace=False)

    def _select_train_samples(self, rows: int) -> np.ndarray:
        """
        Randomly select train sample indices for each tree.

        Parameters
        ----------
        rows : int
            number of data points

        Returns
        -------
        np.ndarray
            seleted train sample indices

        """
        # Determine sample size
        if self.max_samples is None:
            n_samples = min(100, rows)
        elif isinstance(self.max_samples, float) and 0 < self.max_samples < 1:
            n_samples = int(self.max_samples * rows)
        else:
            n_samples = int(min(self.max_samples, rows))

        # pylint: disable=no-member
        rng = np.random.RandomState(42)
        return rng.choice(rows, n_samples, replace=False)

    def fit(self, x: np.ndarray) -> "RobustRandomCutForest":
        """
        Build the forest from training data.

        Parameters
        ----------
        x : np.ndarray
            input data to train

        Returns
        -------
        Class instance
            RobustRandomCutForest
        """
        logger.info("max_samples parameter => %d", self.max_samples)
        logger.info("max_features parameter => %d", self.max_features)

        rows, cols = x.shape
        train_indices = self._select_train_samples(rows)
        self._train_samples = x[train_indices, :]
        logger.info(
            "training samples (%d): indices %s",
            self._train_samples.shape[0],
            train_indices,
        )

        self.n_features_in_ = cols
        self._feature_indices = self._select_features(cols)  # type: ignore[assignment]
        logger.info(
            "training features (%d): indices %s",
            len(self._feature_indices),  # type: ignore[arg-type]
            self._feature_indices,
        )

        x_sub = self._train_samples[:, self._feature_indices]  # type: ignore[index]

        # Building Tree
        self.forest = []
        self._train_scores = np.zeros(self._train_samples.shape[0])
        for _ in tqdm(range(self.num_trees), desc="Building trees"):
            tree = rrcf.RCTree()
            self.forest.append(tree)
            for index, point in enumerate(x_sub):
                tree.insert_point(point, index=index)
                self._train_scores[index] += tree.codisp(index)

        self._train_scores /= self.num_trees

        return self

    def decision_function(self, x: np.ndarray) -> np.ndarray:
        """
        Compute anomaly scores for input data.

        Parameters
        ----------
        x : np.ndarray
            input data for decision

        Returns
        -------
        np.ndarray
            anomalious scores, lower = less anomalous, higher = more anomalous

        """
        # Score aggrigation in tree by parallel job.
        logger.info(
            "RRCF decision_function may be too slow so takes long time.\n \
            Advice: Make sure your max_features and max_samples are what you really need."
        )

        x_sub = x[:, self._feature_indices]  # type: ignore[index]
        n_points = x_sub.shape[0]

        # Create batches
        # batch_size: Number of points to process at once, tree_size = 256 by default
        batch_size = self.tree_size
        batches = [
            (start, min(start + batch_size, n_points))
            for start in range(0, n_points, batch_size)
        ]

        logger.debug("RRCF current forest structure is following. \n{self.forest}")

        # Process trees in parallel
        # n_jobs: Number of parallel jobs (-1 = use all cores)
        tree_scores = Parallel(n_jobs=-1)(
            delayed(self._process_tree)(tree, x_sub, batches)
            for tree in tqdm(self.forest, desc=f"Scoring trees per {batch_size} rows")
        )

        # Aggregate scores
        scores = np.sum(tree_scores, axis=0) / self.num_trees
        return scores

    def _process_tree(
        self, tree: rrcf.RCTree, x_sub: np.ndarray, batches: list
    ) -> np.ndarray:
        """
        Process a single tree with batched operations.

        Parameters
        ----------
        tree: rrcf.RCTree
            rrcf tree object
        x_sub: np.ndarray
            input data of selected features
        batches: list
            batch processing list

        Returns
        -------
        np.ndarray
            RRCF CoDisp scores updated with tree

        """
        scores = np.zeros(x_sub.shape[0])
        for start, end in batches:
            batch = x_sub[start:end]
            temp_indices = np.arange(1000000 + start, 1000000 + end)

            # Insert batch
            for idx, point in zip(temp_indices, batch):
                tree.insert_point(point, index=idx)

            # Calculate CoDisp
            for i, idx in enumerate(temp_indices):
                scores[start + i] = tree.codisp(idx)

            # Remove batch
            for idx in temp_indices:
                tree.forget_point(idx)

        return scores

    def predict(self, x: np.ndarray | None) -> np.ndarray | None:
        """
        Predict anomaly labels (-1 for outliers, 1 for inliers).

        Parameters
        ----------
        x : np.ndarray
            input data to predict

        Returns
        -------
        np.ndarray
            index number list predicted as anomaly

        """
        if x is None:
            logger.info("Using training data for predict()")
            scores = self._train_scores
        elif x.shape == self._train_samples.shape:
            scores = self._train_scores
        else:
            scores = self.decision_function(x)

        # Create label array
        labels = np.ones(len(scores), dtype=int)

        if scores is not None:
            # Calculate number of anomalies
            n_anomalies = int(np.ceil(len(scores) * self.contamination))
            # Get indices of top anomalies
            anomaly_indices = np.argpartition(scores, -n_anomalies)[-n_anomalies:]
            labels[anomaly_indices] = -1

        return labels

    def get_train_samples(self):
        """
        Accessor to training datasets.

        Parameters
        ----------
        None

        Returns
        -------
        np.ndarray
            training data stored in _train_samples member

        """
        return self._train_samples


# pylint: disable=invalid-name
def identify_outliers_rrcf(
    x: np.ndarray,
    x_predict: np.ndarray,
    contamination: float = 0.05,
    num_trees: int = 100,
    tree_size: int = 256,
    max_features: int | float | None = None,
    max_samples: int | float | None = None,
) -> tuple[RobustRandomCutForest, np.ndarray, np.ndarray | None]:
    """
    Identify outlier items using RobustRandomCutForest.

    MIT License Copyright (c) 2018 kLabUM.
    (https://klabum.github.io/rrcf/).

    Parameters
    ----------
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
    max_samples: int or float, optional
        default: None => { min(100,rows) }
        if you want to use more than 100 samples,
        please specify via this option.

    Returns
    -------
    tuple[RobustRandomCutForest, np.ndarray, np.ndarray]
        RobustRandomCutForest model, x_outliers,
        y_pred_outliers

    """
    # pylint: disable=no-member

    # initiate the model
    clf = RobustRandomCutForest(
        num_trees=num_trees,
        tree_size=tree_size,
        contamination=contamination,
        max_features=max_features,
        max_samples=max_samples,
    )

    # fit and train the model
    clf.fit(x)
    clf.predict(x)

    y_pred_outliers = clf.predict(x_predict)

    x_outliers = x_predict[y_pred_outliers == -1]
    return clf, x_outliers, y_pred_outliers


# pylint: disable=too-many-arguments, too-many-locals
def plot_outlier_results(
    clf: IsolationForest | RobustRandomCutForest,
    x: np.ndarray,
    x_predict: np.ndarray,
    x_outliers: np.ndarray,
    feature_columns: list[int],
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
    feature_columns : list[int]
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
                    axes[i, j].contourf(xx, yy, z, cmap=plt.cm.Blues_r)  # type: ignore[index,attr-defined]

                    b1 = axes[i, j].scatter(  # type: ignore[index]
                        x[:, j], x[:, i], c="white", edgecolor="k"
                    )
                    b2 = axes[i, j].scatter(  # type: ignore[index]
                        x_predict[:, j], x_predict[:, i], c="green", edgecolor="k"
                    )
                    c = axes[i, j].scatter(  # type: ignore[index]
                        x_outliers[:, j], x_outliers[:, i], c="red", marker="x"
                    )

                    xp_max_x = x_predict[:, 0].max() + (x_predict[:, 0].max() / 10)
                    xp_min_x = -x_predict[:, 0].max() / 10
                    xp_max_y = x_predict[:, 1].max() + (x_predict[:, 1].max() / 10)
                    xp_min_y = -x_predict[:, 1].max() / 10

                    axes[i, j].axis(xmin=xp_min_x, xmax=xp_max_x)  # type: ignore[index]
                    axes[i, j].axis(ymin=xp_min_y, ymax=xp_max_y)  # type: ignore[index]
                    axes[i, j].set_xlabel(f"{feature_columns[j]}")  # type: ignore[index]
                    axes[i, j].set_ylabel(f"{feature_columns[i]}")  # type: ignore[index]

                else:
                    # do not show the same features x,y each other.
                    axes[i, j].axis("off")  # type: ignore[index]

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


def remove_common_items(data: pd.DataFrame, columns: list[str]) -> pd.DataFrame:
    """
    Remove rows from input DataFrame.

    Parameters
    ----------
    data : pd.DataFrame
        Input dataframe
    columns : list[str]
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
