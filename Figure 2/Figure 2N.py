
import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from scipy import stats


def significance_label(p_value):
    """Convert a p-value to a significance label."""
    if p_value < 0.0001:
        return "****"
    if p_value < 0.001:
        return "***"
    if p_value < 0.01:
        return "**"
    if p_value < 0.05:
        return "*"
    return "ns"


def add_significance_bracket(ax, x1, x2, y, label):
    """Add a comparison bracket and significance label."""
    bracket_height = 0.06
    text_offset = 0.02

    ax.plot(
        [x1, x1, x2, x2],
        [y, y + bracket_height, y + bracket_height, y],
        color="black",
        linewidth=1.1,
        clip_on=False,
    )
    ax.text(
        (x1 + x2) / 2,
        y + bracket_height + text_offset,
        label,
        ha="center",
        va="bottom",
        fontsize=10,
        fontweight="bold",
    )


def read_input_file(file_path, sheet_name=0):
    """Read a CSV or Excel file."""
    path = Path(file_path)

    if not path.exists():
        raise FileNotFoundError(f"File not found: {path}")

    suffix = path.suffix.lower()

    if suffix == ".csv":
        return pd.read_csv(path)
    if suffix in {".xlsx", ".xls"}:
        return pd.read_excel(path, sheet_name=sheet_name)

    raise ValueError("Input file must be a CSV or Excel file.")


def main():
    parser = argparse.ArgumentParser(
        description="Plot Empty-mean-normalized LUC/REN data from a CSV or Excel file."
    )
    parser.add_argument(
        "input_file",
        help="Input CSV or Excel file.",
    )
    parser.add_argument(
        "--sheet",
        default="0",
        help="Excel sheet name or zero-based sheet index. Default: 0.",
    )
    parser.add_argument(
        "--paired",
        action="store_true",
        help="Use a paired t-test. Rows must represent matched replicates.",
    )
    parser.add_argument(
        "--output-prefix",
        default="normalized_LUC_REN",
        help="Output filename prefix. Default: normalized_LUC_REN.",
    )
    args = parser.parse_args()

    sheet_name = int(args.sheet) if args.sheet.isdigit() else args.sheet
    data = read_input_file(args.input_file, sheet_name=sheet_name)
    data.columns = [str(column).strip() for column in data.columns]

    empty_columns = [
        column
        for column in data.columns
        if column.lower().endswith("_empty")
    ]

    if not empty_columns:
        raise ValueError(
            "No control columns found. Name control columns with the "
            "'_Empty' suffix, for example 'PERK_Empty'."
        )

    comparisons = []
    for empty_column in empty_columns:
        treatment_name = empty_column[:-len("_Empty")]
        if treatment_name not in data.columns:
            raise ValueError(
                f"Missing treatment column '{treatment_name}' "
                f"for control column '{empty_column}'."
            )
        comparisons.append((treatment_name, empty_column))

    plt.rcParams["font.family"] = "Arial"
    plt.rcParams["axes.linewidth"] = 1.0

    n_panels = len(comparisons)
    fig, axes = plt.subplots(
        1,
        n_panels,
        figsize=(3.0 * n_panels, 3.3),
        squeeze=False,
    )
    axes = axes[0]

    control_color = "#8DA2CC"
    treatment_color = "#61C2A7"

    for ax, (treatment_name, empty_column) in zip(axes, comparisons):
        control_series = pd.to_numeric(data[empty_column], errors="coerce")
        treatment_series = pd.to_numeric(
            data[treatment_name],
            errors="coerce",
        )

        if args.paired:
            matched = pd.DataFrame(
                {
                    "control": control_series,
                    "treatment": treatment_series,
                }
            ).dropna()

            control_raw = matched["control"].to_numpy(dtype=float)
            treatment_raw = matched["treatment"].to_numpy(dtype=float)

            if len(control_raw) < 2:
                raise ValueError(
                    f"At least two complete matched pairs are required for "
                    f"'{treatment_name}'."
                )
        else:
            control_raw = control_series.dropna().to_numpy(dtype=float)
            treatment_raw = treatment_series.dropna().to_numpy(dtype=float)

            if len(control_raw) < 2 or len(treatment_raw) < 2:
                raise ValueError(
                    f"At least two valid measurements are required in both "
                    f"groups for '{treatment_name}'."
                )

        control_mean_raw = np.mean(control_raw)
        if np.isclose(control_mean_raw, 0):
            raise ValueError(
                f"The mean of '{empty_column}' is zero; normalization is not possible."
            )

        control_normalized = control_raw / control_mean_raw
        treatment_normalized = treatment_raw / control_mean_raw

        means = [
            np.mean(control_normalized),
            np.mean(treatment_normalized),
        ]
        sems = [
            stats.sem(control_normalized),
            stats.sem(treatment_normalized),
        ]

        ax.bar(
            [0, 1],
            means,
            yerr=sems,
            width=0.55,
            color=[control_color, treatment_color],
            edgecolor="none",
            capsize=4,
            error_kw={
                "elinewidth": 1.1,
                "capthick": 1.1,
            },
            zorder=2,
        )

        if args.paired:
            test_result = stats.ttest_rel(treatment_raw, control_raw)
        else:
            test_result = stats.ttest_ind(
                treatment_raw,
                control_raw,
                equal_var=False,
            )

        label = significance_label(test_result.pvalue)
        bracket_y = max(
            means[0] + sems[0],
            means[1] + sems[1],
        ) + 0.10
        add_significance_bracket(ax, 0, 1, bracket_y, label)

        ax.set_xticks([0, 1])
        ax.set_xticklabels(
            ["Empty", treatment_name],
            rotation=45,
            ha="right",
            rotation_mode="anchor",
        )
        ax.set_ylabel("Relative LUC/REN", fontsize=9)
        ax.set_yticks([0, 1, 2, 3])
        ax.tick_params(
            axis="both",
            labelsize=8,
            direction="out",
            length=3,
        )
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)

        upper_limit = max(3.0, bracket_y + 0.30)
        ax.set_ylim(0, upper_limit)

        print(
            f"{treatment_name}: "
            f"Empty = {means[0]:.3f} ¡À {sems[0]:.3f}, "
            f"{treatment_name} = {means[1]:.3f} ¡À {sems[1]:.3f}, "
            f"p = {test_result.pvalue:.4g}, "
            f"significance = {label}"
        )

    fig.tight_layout(w_pad=0.8)

    output_prefix = Path(args.output_prefix)
    output_prefix.parent.mkdir(parents=True, exist_ok=True)

    png_path = output_prefix.with_suffix(".png")
    pdf_path = output_prefix.with_suffix(".pdf")

    fig.savefig(png_path, dpi=600, bbox_inches="tight")
    fig.savefig(pdf_path, bbox_inches="tight")
    plt.show()

    print(f"PNG saved to: {png_path.resolve()}")
    print(f"PDF saved to: {pdf_path.resolve()}")


if __name__ == "__main__":
    main()
