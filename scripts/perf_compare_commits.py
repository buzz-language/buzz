#!/usr/bin/env python3
import argparse
import csv
import datetime as dt
import json
import math
import os
import pathlib
import shutil
import subprocess
import sys
import tempfile


FIRST_RUN_SLOW_WARNING = "first benchmarking run for this command was significantly slower"


BENCHMARKS = [
    {
        "name": "006_vm_arithmetic_dispatch",
        "path": "tests/perf/006_vm_arithmetic_dispatch.buzz",
    },
    {
        "name": "009_vm_object_properties",
        "path": "tests/perf/009_vm_object_properties.buzz",
    },
    {
        "name": "013_vm_concat_clone",
        "path": "tests/perf/013_vm_concat_clone.buzz",
    },
    {
        "name": "014_jit_cheap_hotspot",
        "path": "tests/perf/014_jit_cheap_hotspot.buzz",
    },
    {
        "name": "015_jit_object_heavy_hotspot",
        "path": "tests/perf/015_jit_object_heavy_hotspot.buzz",
    },
    {
        "name": "bench_001_btree_depth14",
        "path": "tests/bench/001-btree.buzz",
        "args": ["14"],
    },
    {
        "name": "bench_002_merkle_depth12",
        "path": "tests/bench/002-merkle.buzz",
        "args": ["12"],
    },
    {
        "name": "bench_005_k_nucleoide",
        "path": "tests/bench/005-k-nucleoide.buzz",
        "stdin": "tests/bench/reference/knucleotide-input.txt",
    },
    {
        "name": "bench_007_fib",
        "path": "tests/bench/007-fib.buzz",
    },
    {
        "name": "bench_008_for",
        "path": "tests/bench/008-for.buzz",
    },
    {
        "name": "bench_009_grid_1000x800",
        "path": "tests/bench/009-grid.buzz",
        "args": ["1000", "800"],
    },
    {
        "name": "bench_010_ackermann_3_9",
        "path": "tests/bench/010-ackermann.buzz",
        "args": ["3", "9"],
    },
    {
        "name": "bench_011_bubble_sort_3000",
        "path": "tests/bench/011-bubble-sort.buzz",
        "args": ["3000"],
    },
]


def run(args, cwd=None, capture=False, check=True):
    result = subprocess.run(
        args,
        cwd=cwd,
        check=check,
        text=True,
        stdout=subprocess.PIPE if capture else None,
        stderr=subprocess.PIPE if capture else None,
    )
    return result.stdout.strip() if capture else ""


def require_cmd(name):
    if shutil.which(name) is None:
        raise SystemExit(f"error: missing required command: {name}")


def split_env(name, default):
    value = os.environ.get(name, default)
    return value.split() if value else []


def commit_subject(repo, commit):
    return run(["git", "log", "-1", "--format=%s", commit], cwd=repo, capture=True)


def short_sha(repo, commit):
    return run(["git", "rev-parse", "--short=12", commit], cwd=repo, capture=True)


def commit_range(repo, start_sha, head_sha):
    has_parent = subprocess.run(
        ["git", "rev-parse", "--verify", f"{start_sha}^"],
        cwd=repo,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    ).returncode == 0

    if has_parent:
        out = run(
            ["git", "rev-list", "--reverse", "--ancestry-path", f"{start_sha}^..{head_sha}"],
            cwd=repo,
            capture=True,
        )
        return [line for line in out.splitlines() if line]

    out = run(
        ["git", "rev-list", "--reverse", "--ancestry-path", f"{start_sha}..{head_sha}"],
        cwd=repo,
        capture=True,
    )
    return [start_sha] + [line for line in out.splitlines() if line]


def benchmark_command_for(benchmark):
    args = " ".join(benchmark.get("args", []))
    if args:
        args = f" {args}"

    stdin = benchmark.get("stdin")
    stdin_redirect = f" < {stdin}" if stdin else ""

    return f"./zig-out/bin/buzz {benchmark['path']}{args}{stdin_redirect} >/dev/null"


def benchmark_source_paths():
    paths = []
    seen = set()
    for benchmark in BENCHMARKS:
        for key in ("path", "stdin"):
            path = benchmark.get(key)
            if path and path not in seen:
                seen.add(path)
                paths.append(path)
    return paths


def copy_benchmark_sources(repo, destination):
    for relative in benchmark_source_paths():
        source = repo / relative
        target = destination / relative
        if not source.is_file():
            raise SystemExit(f"error: missing benchmark source: {relative}")

        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, target)


def hyperfine_warnings(output):
    warnings = []

    for line in output.splitlines():
        stripped = line.strip()
        if stripped.startswith("Warning:"):
            warnings.append(stripped.removeprefix("Warning:").strip())

    return warnings


def has_first_run_slow_warning(output):
    return FIRST_RUN_SLOW_WARNING in output.lower()


def run_hyperfine(hyperfine_cmd, cwd, name):
    attempts = []

    for attempt in range(1, 3):
        result = subprocess.run(
            hyperfine_cmd,
            cwd=cwd,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        output = result.stdout + result.stderr
        print(output, end="")

        warnings = hyperfine_warnings(output)
        attempts.append(
            {
                "returncode": result.returncode,
                "warnings": warnings,
            }
        )

        if result.returncode != 0:
            return result.returncode, attempts

        if attempt == 1 and has_first_run_slow_warning(output):
            print(
                f"warning: benchmark `{name}` had a slow first run; rerunning once",
                file=sys.stderr,
            )
            continue

        return 0, attempts

    return 0, attempts


def merge_hyperfine_result(source_json, combined_results, attempts):
    with source_json.open() as f:
        payload = json.load(f)

    reran = len(attempts) > 1
    warnings = attempts[-1]["warnings"] if attempts else []

    for result in payload.get("results", []):
        result["hyperfine_warnings"] = warnings
        result["reran_after_first_run_warning"] = reran
        combined_results.append(result)


def update_submodules(worktree):
    run(["git", "-C", str(worktree), "submodule", "sync", "--recursive"])
    run(["git", "-C", str(worktree), "submodule", "update", "--init", "--recursive"])


def fmt_seconds(value):
    if value is None or math.isnan(value):
        return ""
    if value >= 1:
        return f"{value:.3f}s"
    if value >= 0.001:
        return f"{value * 1000:.2f}ms"
    return f"{value * 1_000_000:.2f}us"


def gain(base, current):
    if base is None or current is None or base == 0:
        return None
    return (base - current) / base * 100.0


def coefficient_of_variation(result):
    if result is None:
        return None

    mean = result.get("mean")
    stddev = result.get("stddev")
    if mean is None or stddev is None or mean == 0:
        return None

    return stddev / mean


def fmt_percent(value):
    return "" if value is None else f"{value:+.2f}%"


def fmt_cv(result):
    cv = coefficient_of_variation(result)
    return "" if cv is None else f"{cv * 100:.1f}%"


def unstable_result(result):
    if result is None:
        return False

    cv = coefficient_of_variation(result)
    has_warning = len(result.get("hyperfine_warnings", [])) > 0

    return has_warning or (cv is not None and cv > 0.20)


def fmt_warnings(result):
    if result is None:
        return ""

    warnings = result.get("hyperfine_warnings", [])
    if not warnings:
        return ""

    return "; ".join(warnings)


def load_results(commits):
    data = {}
    failures = {}
    tests = [benchmark["name"] for benchmark in BENCHMARKS]
    for commit in commits:
        with commit["json"].open() as f:
            payload = json.load(f)

        per_test = {}
        for result in payload.get("results", []):
            name = result["command"]
            per_test[name] = result

        data[commit["short"]] = per_test
        failures[commit["short"]] = {
            failure["name"]: failure
            for failure in payload.get("failures", [])
        }

    return data, failures, tests


def write_reports(results_dir, commits):
    data, failures, tests = load_results(commits)

    csv_path = results_dir / "all-results.csv"
    with csv_path.open("w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(
            [
                "commit",
                "short",
                "subject",
                "test",
                "mean_seconds",
                "stddev_seconds",
                "median_seconds",
                "gain_vs_start_percent",
                "gain_vs_previous_percent",
                "median_gain_vs_start_percent",
                "median_gain_vs_previous_percent",
                "user_seconds",
                "user_gain_vs_start_percent",
                "user_gain_vs_previous_percent",
                "system_seconds",
                "system_gain_vs_start_percent",
                "system_gain_vs_previous_percent",
                "coefficient_of_variation",
                "unstable",
                "hyperfine_warnings",
                "reran_after_first_run_warning",
            ]
        )

        previous_short = None
        base_short = commits[0]["short"]
        for commit in commits:
            short = commit["short"]
            for test in tests:
                result = data.get(short, {}).get(test)
                base = data.get(base_short, {}).get(test)
                previous = data.get(previous_short, {}).get(test) if previous_short else None

                mean = result.get("mean") if result else None
                base_mean = base.get("mean") if base else None
                previous_mean = previous.get("mean") if previous else None
                median = result.get("median") if result else None
                base_median = base.get("median") if base else None
                previous_median = previous.get("median") if previous else None
                user = result.get("user") if result else None
                base_user = base.get("user") if base else None
                previous_user = previous.get("user") if previous else None
                system = result.get("system") if result else None
                base_system = base.get("system") if base else None
                previous_system = previous.get("system") if previous else None

                writer.writerow(
                    [
                        commit["sha"],
                        short,
                        commit["subject"],
                        test,
                        mean,
                        result.get("stddev") if result else None,
                        result.get("median") if result else None,
                        gain(base_mean, mean),
                        gain(previous_mean, mean),
                        gain(base_median, median),
                        gain(previous_median, median),
                        user,
                        gain(base_user, user),
                        gain(previous_user, user),
                        system,
                        gain(base_system, system),
                        gain(previous_system, system),
                        coefficient_of_variation(result),
                        unstable_result(result),
                        fmt_warnings(result),
                        result.get("reran_after_first_run_warning") if result else None,
                    ]
                )
            previous_short = short

    summary_path = results_dir / "summary.md"
    base = commits[0]
    head = commits[-1]
    with summary_path.open("w") as f:
        f.write("# Buzz perf comparison\n\n")
        f.write(f"Start: `{base['short']}` {base['subject']}\n\n")
        f.write(f"Head: `{head['short']}` {head['subject']}\n\n")
        f.write("Positive gain means the later commit is faster. Summary gains use medians; CV above 20% or any hyperfine warning is flagged as unstable. If hyperfine reports a significantly slower first run, that benchmark is rerun once and the rerun is recorded.\n\n")

        f.write("## HEAD vs start\n\n")
        f.write("| Test | Start median | HEAD median | Median gain | Start user | HEAD user | User gain | HEAD CV | HEAD warnings |\n")
        f.write("| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |\n")
        for test in tests:
            base_result = data.get(base["short"], {}).get(test)
            head_result = data.get(head["short"], {}).get(test)
            base_median = base_result.get("median") if base_result else None
            head_median = head_result.get("median") if head_result else None
            g = gain(base_median, head_median)
            base_user = base_result.get("user") if base_result else None
            head_user = head_result.get("user") if head_result else None
            user_gain = gain(base_user, head_user)
            cv_text = fmt_cv(head_result)
            if unstable_result(head_result):
                cv_text += " unstable"
            warning_text = fmt_warnings(head_result)
            f.write(
                f"| `{test}` | {fmt_seconds(base_median)} | {fmt_seconds(head_median)} | {fmt_percent(g)} | {fmt_seconds(base_user)} | {fmt_seconds(head_user)} | {fmt_percent(user_gain)} | {cv_text} | {warning_text} |\n"
            )

        f.write("\n## Per-commit rundown\n\n")
        for index, commit in enumerate(commits):
            short = commit["short"]
            previous_short = commits[index - 1]["short"] if index > 0 else None

            f.write(f"### `{short}` {commit['subject']}\n\n")

            for test in tests:
                result = data.get(short, {}).get(test)
                if result is None:
                    failure = failures.get(short, {}).get(test)
                    if failure is not None:
                        f.write(f"- `{test}`: failed, exit {failure['returncode']}\n")
                    else:
                        f.write(f"- `{test}`: no result\n")
                    continue

                mean = result.get("mean")
                median = result.get("median")
                stddev = result.get("stddev")
                user = result.get("user")
                system = result.get("system")
                base_result = data.get(base["short"], {}).get(test)
                previous_result = data.get(previous_short, {}).get(test) if previous_short else None

                base_gain = gain(base_result.get("median") if base_result else None, median)
                previous_gain = gain(previous_result.get("median") if previous_result else None, median)
                base_user_gain = gain(base_result.get("user") if base_result else None, user)
                previous_user_gain = gain(previous_result.get("user") if previous_result else None, user)

                parts = [
                    f"mean {fmt_seconds(mean)}",
                    f"median {fmt_seconds(median)}",
                    f"stddev {fmt_seconds(stddev)}",
                    f"user {fmt_seconds(user)}",
                    f"system {fmt_seconds(system)}",
                ]

                cv_text = fmt_cv(result)
                if cv_text:
                    if unstable_result(result):
                        cv_text += " unstable"
                    parts.append(f"CV {cv_text}")

                if result.get("reran_after_first_run_warning"):
                    parts.append("reran after slow first run")

                warning_text = fmt_warnings(result)
                if warning_text:
                    parts.append(f"hyperfine warning: {warning_text}")

                if previous_gain is not None:
                    parts.append(f"median vs previous {previous_gain:+.2f}%")

                if base_gain is not None:
                    parts.append(f"median vs start {base_gain:+.2f}%")

                if previous_user_gain is not None:
                    parts.append(f"user vs previous {previous_user_gain:+.2f}%")

                if base_user_gain is not None:
                    parts.append(f"user vs start {base_user_gain:+.2f}%")

                f.write(f"- `{test}`: " + ", ".join(parts) + "\n")

            f.write("\n")

    return summary_path, csv_path


def parse_args():
    parser = argparse.ArgumentParser(
        description=(
            "Check out each commit from START_COMMIT to HEAD, build Buzz, "
            "run the selected Buzz perf benchmarks with hyperfine, and compare results."
        )
    )
    parser.add_argument(
        "start_commit",
        nargs="?",
        default="HEAD^",
        help="first commit to benchmark, default: HEAD^",
    )
    parser.add_argument(
        "--results-dir",
        default=os.environ.get("RESULTS_DIR"),
        help="output directory, default: perf-results-<timestamp>",
    )
    parser.add_argument(
        "--build-args",
        default=os.environ.get("BUZZ_BUILD_ARGS", "-Doptimize=ReleaseFast"),
        help='arguments after "zig build", default: %(default)s',
    )
    parser.add_argument(
        "--warmup",
        type=int,
        default=int(os.environ.get("HYPERFINE_WARMUP", "5")),
        help="hyperfine warmup count",
    )
    parser.add_argument(
        "--runs",
        type=int,
        default=int(os.environ.get("HYPERFINE_RUNS", "10")),
        help="hyperfine run count",
    )
    parser.add_argument(
        "--hyperfine-extra-args",
        default=os.environ.get("HYPERFINE_EXTRA_ARGS", ""),
        help="extra arguments passed to hyperfine",
    )
    return parser.parse_args()


def main():
    args = parse_args()

    for cmd in ("git", "zig", "hyperfine"):
        require_cmd(cmd)

    repo = pathlib.Path(run(["git", "rev-parse", "--show-toplevel"], capture=True))
    os.chdir(repo)

    for relative in benchmark_source_paths():
        if not (repo / relative).is_file():
            raise SystemExit(f"error: benchmark source does not exist: {relative}")

    start_sha = run(["git", "rev-parse", "--verify", f"{args.start_commit}^{{commit}}"], cwd=repo, capture=True)
    head_sha = run(["git", "rev-parse", "--verify", "HEAD"], cwd=repo, capture=True)

    ancestor = subprocess.run(
        ["git", "merge-base", "--is-ancestor", start_sha, head_sha],
        cwd=repo,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    if ancestor.returncode != 0:
        raise SystemExit(f"error: {start_sha} is not an ancestor of HEAD")

    commits_to_run = commit_range(repo, start_sha, head_sha)
    if not commits_to_run:
        raise SystemExit("error: empty commit range")

    timestamp = dt.datetime.now().strftime("%Y%m%d-%H%M%S")
    results_dir = pathlib.Path(args.results_dir) if args.results_dir else repo / f"perf-results-{timestamp}"
    json_dir = results_dir / "json"
    json_dir.mkdir(parents=True, exist_ok=True)

    build_args = args.build_args.split() if args.build_args else []
    hyperfine_extra_args = args.hyperfine_extra_args.split() if args.hyperfine_extra_args else []

    with tempfile.TemporaryDirectory(prefix="buzz-perf-commits.") as tmp:
        tmp_path = pathlib.Path(tmp)
        worktree = tmp_path / "worktree"
        benchmark_snapshot = tmp_path / "benchmarks"

        copy_benchmark_sources(repo, benchmark_snapshot)
        tests = BENCHMARKS

        run(["git", "worktree", "add", "--detach", "--quiet", str(worktree), head_sha], cwd=repo)

        commands_path = results_dir / "commands.tsv"
        commits_path = results_dir / "commits.tsv"
        completed_commits = []

        try:
            with commands_path.open("w", newline="") as commands_file, commits_path.open("w", newline="") as commits_file:
                commands_writer = csv.writer(commands_file, delimiter="\t")
                commits_writer = csv.writer(commits_file, delimiter="\t")

                print(f"Benchmarking {len(commits_to_run)} commits with {len(tests)} tests each")
                print(f"Results: {results_dir}")
                print(f"Build args: zig build {' '.join(build_args)}")
                print("Benchmarks:")
                for benchmark in tests:
                    print(f"  - {benchmark['name']}: {benchmark_command_for(benchmark)}")

                for idx, commit in enumerate(commits_to_run, start=1):
                    short = short_sha(repo, commit)
                    subject = commit_subject(repo, commit)
                    json_file = json_dir / f"{idx:04d}-{short}.json"

                    print(f"\n[{idx}/{len(commits_to_run)}] {short} {subject}")

                    run(["git", "-C", str(worktree), "checkout", "--force", "--quiet", commit], cwd=repo)
                    update_submodules(worktree)
                    copy_benchmark_sources(benchmark_snapshot, worktree)

                    run(["zig", "build", *build_args], cwd=worktree)

                    commits_writer.writerow([idx, commit, short, subject, json_file])
                    commits_file.flush()

                    combined_results = []
                    failures = []

                    for benchmark in tests:
                        name = benchmark["name"]
                        command = benchmark_command_for(benchmark)
                        benchmark_json = json_dir / f"{idx:04d}-{short}-{name}.json"
                        commands_writer.writerow([short, name, command])
                        commands_file.flush()

                        hyperfine_cmd = [
                            "hyperfine",
                            "--warmup",
                            str(args.warmup),
                            "--runs",
                            str(args.runs),
                            "--export-json",
                            str(benchmark_json),
                            *hyperfine_extra_args,
                            "--command-name",
                            name,
                            command,
                        ]

                        returncode, attempts = run_hyperfine(hyperfine_cmd, worktree, name)
                        if returncode == 0:
                            merge_hyperfine_result(benchmark_json, combined_results, attempts)
                        else:
                            failures.append(
                                {
                                    "name": name,
                                    "command": command,
                                    "returncode": returncode,
                                    "attempts": attempts,
                                }
                            )
                            print(
                                f"warning: benchmark `{name}` failed with exit code {returncode}; continuing",
                                file=sys.stderr,
                            )

                    with json_file.open("w") as f:
                        json.dump(
                            {
                                "results": combined_results,
                                "failures": failures,
                            },
                            f,
                            indent=2,
                        )

                    completed_commits.append(
                        {
                            "idx": idx,
                            "sha": commit,
                            "short": short,
                            "subject": subject,
                            "json": json_file,
                        }
                    )
        finally:
            run(["git", "worktree", "remove", "--force", str(worktree)], cwd=repo, check=False)

    if not completed_commits:
        raise SystemExit("error: no benchmark results were recorded")

    summary, csv_path = write_reports(results_dir, completed_commits)
    print("\nDone.")
    print(f"Summary: {summary}")
    print(f"CSV:     {csv_path}")


if __name__ == "__main__":
    try:
        main()
    except subprocess.CalledProcessError as err:
        cmd = " ".join(str(part) for part in err.cmd)
        print(f"error: command failed: {cmd}", file=sys.stderr)
        if err.stdout:
            print(err.stdout, file=sys.stderr)
        if err.stderr:
            print(err.stderr, file=sys.stderr)
        raise SystemExit(err.returncode)
