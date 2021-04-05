#!/usr/bin/python3

# generate all exprs that are one level deep
# (define-type expr (U binopE ifE Natural Boolean))

def get_nums():
	nums = []
	for i in range(0, 3):
		nums.append(str(i))
	return nums

def format_binop(op, left, right):
	return "\'(" + op + " " + left + " " + right + ")"

# binops of the form (-> num num num)
def arith_binop():
	binops = []
	ops = ["+", "*"]
	nums1 = get_nums()
	nums2 = get_nums()

	for i in range(0, len(ops)):
		for j in range(0, len(nums1)):
			for k in range(0, len(nums2)):
				binops.append(format_binop(ops[i], nums1[j], nums2[k]))

	return binops

# binops of the form (-> binop num num)
def arith_binop_left():
	binops = []
	ops = ["+", "*"]
	nums1 = get_nums()

	left_children = arith_binop()

	for i in range(0, len(ops)):
		for j in range(0, len(left_children)):
			for k in range(0, len(nums1)):
				binops.append(format_binop(ops[i], left_children[j], nums1[k]))

	return binops

# binops of the form (-> num binop num)
def arith_binop_right():
	binops = []
	ops = ["+", "*"]
	nums1 = get_nums()

	right_children = arith_binop()

	for i in range(0, len(ops)):
		for j in range(0, len(nums1)):
			for k in range(0, len(right_children)):
				binops.append(format_binop(ops[i], nums1[j], right_children[k]))

	return binops

# only going 2 deep
# binops of the form (-> binop binop num)
def arith_binop_both():
	binops = []
	ops = ["+", "*"]

	left_children = arith_binop()
	right_children = arith_binop()

	for i in range(0, len(ops)):
		for j in range(0, len(left_children)):
			for k in range(0, len(right_children)):
				binops.append(format_binop(ops[i], left_children[j], right_children[k]))

	return binops

def print_nice(alist):
	for item in alist:
		print(item)

def get_all_programs():
	return get_nums() + arith_binop() + arith_binop_left() + arith_binop_right() + arith_binop_both()

# print_nice(get_nums())
# print_nice(arith_binop_left())
# print_nice(arith_binop_right())

# print(len(arith_binop()))
# print(len(arith_binop_left()))
# print(len(arith_binop_right()))
# print(len(arith_binop_both()))
