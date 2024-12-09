"""Advent of Code 2024 - Day 9."""

from dataclasses import dataclass
from sys import stdin


@dataclass
class Chunk:
    """One chunk on disk: file or free space."""

    id_: int | None  # File id (or None for free-space)
    blocks: int  # Number of blocks in the file
    index: int  # Index of first block of the file


def read_disk(s: str) -> list[Chunk]:
    """Read disk string into a list."""
    acc: list[Chunk] = []
    file_index: int = 0
    for i, c in enumerate(s):
        blocks = int(c)
        if i % 2 == 0:
            id_: int | None = i // 2
        else:
            id_ = None
        acc.append(Chunk(id_=id_, blocks=blocks, index=file_index))
        file_index += blocks
    return acc


def compact(disk: list[Chunk]) -> int:
    """Compact the disk and return the checksum."""
    left_chunk: int = 0  # Chunk we are currently summing
    right_chunk: int = len(disk) - 1  # Chunk we are transferring from
    assert disk[right_chunk].id_ is not None  # Check last chunk is a file

    total_file_blocks = sum(d.blocks for d in disk if d.id_ is not None)

    current_block: int = 0
    check_sum: int = 0
    left_summed: int = 0  # Blocks already check-summed from left_chunk
    right_transferred: int = 0  # Blocks transferred from right_chunk

    while current_block < total_file_blocks:
        assert disk[left_chunk].blocks != 0
        assert disk[right_chunk].blocks != 0
        match disk[left_chunk].id_:
            case None:
                right_file_id = disk[right_chunk].id_
                assert right_file_id is not None
                check_sum += right_file_id * current_block
                right_transferred += 1
                if right_transferred >= disk[right_chunk].blocks:
                    right_chunk -= 2
                    right_transferred = 0
            case file_id:
                check_sum += file_id * current_block
        left_summed += 1
        while left_summed >= disk[left_chunk].blocks:
            left_chunk += 1
            left_summed = 0
        current_block += 1
    return check_sum


def find_right_most_fit(
    current_chunk: int,
    space: int,
    disk: list[Chunk],  # , used: list[bool]
) -> tuple[int, int, int] | None:
    """Return index, id and blocks of right-most chunk whose size fits."""
    for i in range(len(disk) - 1, current_chunk, -1):
        # if not used[i]:
        i_id = disk[i].id_
        if i_id is not None and disk[i].blocks <= space:
            return (i, i_id, disk[i].blocks)
    return None


def compact_whole(disk: list[Chunk]) -> int:
    """Compact the disk with whole files and return the checksum."""
    current_chunk = 0
    current_block = 0
    check_sum = 0

    while current_chunk < len(disk):
        blocks = disk[current_chunk].blocks
        match disk[current_chunk].id_:
            case None:
                while blocks > 0:
                    match find_right_most_fit(current_chunk, blocks, disk):  # , used):
                        case None:
                            current_block += blocks
                            break
                        case (r, r_id, r_blocks):
                            for _ in range(r_blocks):
                                check_sum += r_id * current_block
                                current_block += 1
                                blocks -= 1
                            disk[r].id_ = None  # Mark block as free space
            case file_id:
                for _ in range(blocks):
                    check_sum += file_id * current_block
                    current_block += 1
        current_chunk += 1
    return check_sum


if __name__ == "__main__":
    disk = read_disk(stdin.read().strip())
    print(compact(disk))
    print(compact_whole(disk))
