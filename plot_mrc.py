from os import listdir, makedirs
from os.path import isfile, join
import sys

from pprint import pprint
import matplotlib.pyplot as plt


def load_measurements(sizes, data):
  curves = {}
  for (workload, policy, points) in data:
    if not workload in curves:
      curves[workload] = {}
    curves[workload][policy] = points

  print(pprint(curves))
  return sizes, curves

# measurements: dict[workload]
def plot_measurements(out_dir, sizes, measurements):
  for workload in measurements:
    plt.rcParams["figure.figsize"] = (12, 8)
    plt.rcParams.update({'font.size': 16})

    for policy in measurements[workload]:
      plt.plot(sizes, measurements[workload][policy], label = policy)

    plt.tight_layout()  # otherwise the right y-label is slightly clipped
    plt.title(workload)
    plt.legend(loc="upper right")

    # plt.show()
    plt.savefig(join(out_dir, f"{workload}.png"))
    plt.clf()

if __name__ == "__main__":
    in_file = sys.argv[1]
    sizes = None
    data = None
    with open(in_file, "r") as f:
      sizes = eval(f.readline())
      data = eval(f.readline())
    out_dir = "mrcs"
    makedirs(out_dir, exist_ok=True)

    sizes, measurements = load_measurements(sizes, data)
    plot_measurements(out_dir, sizes, measurements)