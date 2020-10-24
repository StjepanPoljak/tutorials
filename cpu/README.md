# Memory

## Abbreviations

|  abbr.  |         meaning        |
|---------|------------------------|
|   MMU   | Memory Management Unit |
|   PA    |    Physical Address    |
|   VA    |    Virtual Address     |
|  P(T)E  |   Page (Table) Entry   |

## Further notes

The calculations in code snippets use Python3.

The `size_exp` variable is assumed as an entry from this table:

| size | `size_exp` |
|------|------------|
|  GB  |     3      |
|  MB  |     2      |
|  kB  |     1      |
|  B   |     0      |

## Memory basics

### Number of addresses from RAM size

Number of addresses is best described as a power of 2, so use logarithm of base two on RAM bytes to get the exponent.

```python
print('2^{}'.format(
	math.log2(ram_size * (1024**size_exp))
	))
```

### Max. address from RAM size

```python
hex(ram_size * (1024**size_exp))
```

## Paging

 * **fine grain** - 1:1 correspondence between VA and PA
 * **coarse grain** - each VA corresponds to a PTE of fixed size

PTE is usually understood as an entry in the page table, while a page is the corresponding region in the VA space to which PTE points.

### Number of PTEs

The variable `balign` holds the byte alignment value. 

#### From RAM size

```python
print('2^{}'.format(
	math.log2(ram_size * (1024**size_exp) / balign)
	))
```

#### Number of PTEs required from n-bit space

The `n` variable holds how many bits there are in address space. For example, for 32-bit space, `bit_space = 32`.

##### Fine grain

```python
num_ptes = (2**n) / balign
```

##### Coarse grain

The `bgrain` holds page size in bytes (so for a 4kB page, `bgrain = 4096`).

```python
num_ptes = (2**n) / bgrain
```

#### Memory size from number of PTEs

This is dependant on implementation. Let each PTE take up `pte_bytes`. Then, memory size required for a page table is:

```python
page_table_size_b = pte_bytes * num_ptes
```

Combining that from previous example, if we have an `n`-bit address space:

```python
page_table_size_b = (pte_bytes * 2**n) / bgrain
```

Example: Assume we have 32-bit VA space and each page is 4096kB. Also assume that one PTE takes up 4 bytes. Then, memory needed for each page table is `(4 * 2^32) / 4096 = 4194304`. That amounts to `4194304 / (1024**2) = 4MB`. So each process would need at least 4MB in order to accomodate a page table.

#### Page offset bits

To get the number of bits reserved for page offset (that translate directly from VA page entry to PA):

```python
page_offset_bits = math.log2(bgrain)
```

### Dirty page

If PTE has a dirty bit set for a page it points to, that means that the page has been modified and its data must be preserved.

### Page fault

If PTE points to disk, CPU generates a page fault exception. The hardware then jumps to the OS page fault handler. The OS will choose a page to evict from RAM and if the page to be evicted is marked as dirty, it will be saved to disk. The page that is alredy located in the disk (pointed to by PTE) is read to RAM and PTE is updated.
